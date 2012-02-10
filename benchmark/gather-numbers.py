#!/usr/bin/env python

from optparse import OptionParser
import multiprocessing
import os, sys
import math
from socket import gethostname
from string import *
import ConfigParser
from time import localtime, strftime

DELITE_HOME = os.getenv("DELITE_HOME")

apps_default = ['gda', 'nb', 'linreg', 'kmeans', 'rbm', 'svm']#, 'lbp']
delite_threads_default = [1, 2, 4, 8]


#delite_gpus = [ 1, 2 ]
#matlab_apps = []
#c_apps = []




params = {}
classes = {}
props = {}
options = {}


def main():
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")
    parser.add_option("-a", "--apps", dest="apps", default="_all", help="a list of comma separated applications to run (e.g. -a gda,nb)")
    parser.add_option("-r", "--runs", dest="runs", default="10", type="string", help="the number of times the runtime will execute the applications")
    parser.add_option("-t", "--threads", dest="threads", default="_all", help="a list of comma separated thread counts (e.g. -t 1,2,4)")
    parser.add_option("-s", "--skip", dest="skip", default="_none", help="skips smp and/or gpu portion of gathering numbers (e.g. -s gpu)")
    parser.add_option("-k", "--keep-going", dest="keep_going", action="store_true", help="keep going even if there is a abnormal exit code")
    parser.add_option("--input-size", dest="input_size", default="icml", help="specify which dataset input size to use when collecting numbers")
    parser.add_option("--nv", dest="no_variants", action="store_true" , help="disables variant support in the framework")
    parser.add_option("--nb", dest="no_blas", action="store_true", help="disables blas calls in generated code")
    parser.add_option("--nf", dest="no_fusion", action="store_true", help="disables op fusion")
    parser.add_option("--ns", dest="no_stencil", action="store_true", help="disables stencil collection")
    parser.add_option("--globals", dest="print_globals", action="store_true", help="print globals")
    parser.add_option("--home", dest="delite_home", default="_env", help="allows you to specify a different Delite Home than the one that should be specificed in the environment")
    parser.add_option("--stats-dir", dest="stats_dir", default=None, help="allows you to specify a different statistics output directory. environment variables are interpolated")
    parser.add_option("--timestamp", dest="stats_time", action="store_true", help="store statistics under a timestamped directory")
    parser.add_option("-d", "--datasets", dest="datasets", default=None, help="allows you to specify a different datasets file to use. otherwise defaults to benchmark/config/datasets.HOSTNAME.INPUT_SIZE")

    (opts, args) = parser.parse_args()
    if len(args) != 0:
        parser.error("incorrect number of arguments")
    

    loadOptions(opts)
    loadProps(options)
    loadParams(options)
    loadClasses(options)
    
    launchApps(options)

def loadOptions(opts):
    options['verbose'] = opts.verbose
    #process apps
    if(opts.apps == "_all"):
        options['apps'] = apps_default
    else:
        options['apps'] = opts.apps.split(',')
    #process numRuns
    options['runs'] = opts.runs
    #process numThreads
    if(opts.threads == "_all"):
        options['delite.threads'] = delite_threads_default
    else:
        options['delite.threads'] = opts.threads.split(',')
    options['run']={}
    options['run']['smp'] = True
    options['run']['gpu'] = True
    
    if(opts.skip != "_none"):
        for s in opts.skip.split(','):
            options['run'][s] = False
    options['variants'] = not opts.no_variants
    options['blas'] = not opts.no_blas
    options['fusion'] = not opts.no_fusion
    options['stencil'] = not opts.no_stencil
    options['print_globals'] = opts.print_globals

    options['keep-going'] = opts.keep_going
    options['input-size'] = opts.input_size
    
    if opts.datasets:
      options['datasets'] = opts.datasets
      
    #set delite home
    if(opts.delite_home != "_env"):
        props["delite.home"] = opts.delite_home
    else:
        props["delite.home"] = DELITE_HOME

    if props["delite.home"] is None:
        #try to check if it is in the usual place
        script_path = os.path.realpath(__file__)
        candidate_path = script_path 
        candidate_path = candidate_path.replace('/benchmark/gather-numbers.py','',1)
        if os.path.isfile(candidate_path + os.sep + "delite.properties"):
            props['delite.home']= candidate_path
        else:
            exit("DELITE_HOME not defined and delite.properties not at ../ from script, needs to be set to point to Delite home directory")
    stats_dir = opts.stats_dir
    if props['delite.home']:
      stats_dir = stats_dir or props['delite.home']  + "/benchmark/times"
    if opts.stats_time:
      stats_dir = os.path.join(stats_dir, strftime("%Y-%m-%d_%H-%M-%S", localtime()))
    options['stats-dir'] = stats_dir
    
    print """
==============================
==   options
=============================="""
    for k in options.keys():
        print "options[" + k + "] = " + str(options[k])

def loadProps(options):
    #load and check all the required environment variables
    config = ConfigParser.ConfigParser()
    config.readfp(open(props["delite.home"] + '/delite.properties'))
    items = config.items('delite')
    for item in items:
        k, v = item
        props[k] = v
    if(options['verbose']):
        print "loaded the following properties from delite.properties"

    print """
=============================
==  Props
============================="""
    for k in props.keys():
        print k + ":" + props[k]

def launchApps(options):
    # run the delite applications
    for app in options['apps']:
        print "==================================================="
        print "==         " + app 
        print "===================================================" 
        if app not in classes:
            print "Could not find class definition for application %s" % app
            if options['keep-going'] is None:
              exit(-1)
            else:
              continue
        
        java_opts = os.getenv("JAVA_OPTS", "")
        build_dir = props["delite.home"] + "/generated/"
        opts = " -Ddelite.home.dir=" + props["delite.home"] + " -Ddelite.build.dir=" + build_dir + " -Ddelite.deg.filename=" + app + ".deg" + " -Dlms.verbosity=1"
        if options['blas'] == True:
            opts = opts + " -Ddelite.extern.blas"
        if options['run']['gpu'] == True:
            opts = opts + " -Ddelite.generate.cuda"
        if options['variants'] == False:
            opts = opts + " -Dnested.variants.level=0"
        if options['fusion'] == False:
            opts = opts + " -Ddelite.enable.fusion=false"
        opts = opts + " " + java_opts
        os.putenv("JAVA_OPTS", opts)
        os.putenv("GEN_OPTS", opts)
        #MKL ENV
        os.putenv("LD_PRELOAD", props['java.home'] + "/jre/lib/amd64/libjsig.so")
        #emit other envs
        os.putenv("JAVA_HOME", props['java.home'])
        os.putenv("DELITE_HOME", props['delite.home'])
        os.putenv('LMS_HOME', props['libs.lms.home'])
        os.putenv('SCALA_VIRT_HOME', props['scala.virtualized.home'])
        print "==  Generating DEG file with options: " + opts
        print "executing command: " + props['delite.home'] + "/bin/gen " + classes[app] + " " + params[app]
        ecode = os.system(props['delite.home'] + "/bin/gen " + classes[app] + " " + params[app])
        if ecode != 0 and options['keep-going'] == None:
            print "Detected abnormal exit code, exiting"
            exit(-1)
        #do it for each config of delite
        #do it for each thread configuration
        ld_library_path = filter(len, os.getenv("LD_LIBRARY_PATH", "").split(":"))
        ld_library_path.append(build_dir+"/libraries")
        os.putenv("LD_LIBRARY_PATH", ":".join(ld_library_path))
        if options['run']['smp']: 
            for numThreads in options['delite.threads']:
                os.putenv("MKL_NUM_THREADS", str(numThreads))
                os.putenv("SCALA_HOME", props['scala.vanilla.home'])
                
                stats_dir = os.path.expandvars(options['stats-dir'])
                
                opts = "-Ddelite.home=" + props['delite.home'] + " -Ddelite.threads=" + str(numThreads) + " -Ddelite.runs=" + options['runs'] + " -Dstats.dump -Dstats.dump.component=app -Dstats.dump.overwrite -Dstats.output.dir=" + stats_dir + " -Dstats.output.filename=" + app + "-smp-" +str(numThreads) + ".times " + java_opts
                os.putenv("JAVA_OPTS", opts)
                
                print "== executing application: " + app + " " + params[app],
                print "== with options: " + opts + "\n"
                print props['delite.home'] + "/bin/exec " + app + ".deg " + params[app]
                ecode = os.system(props['delite.home'] + "/bin/exec " + app + ".deg " + params[app])
                if ecode != 0 and options['keep-going'] == None:
                    print "Detected abnormal exit code, exiting"
                    exit(-1)

        #check if gpu option is enabled
        if options['run']['gpu']:
            os.putenv("MKL_NUM_THREADS", "1")
            #need nvcc in your path
            os.putenv('PATH', props['nvidia.cuda'] + "/bin:" + os.getenv('PATH'))
            os.putenv("SCALA_HOME", props['scala.vanilla.home'])
            
            stats_dir = os.path.expandvars(options['stats-dir'])
            
            opts = "-Ddelite.home=" + props['delite.home'] +" -Ddelite.threads=1 -Ddelite.gpus=1 -Ddelite.runs=" + options['runs'] + " -Dstats.dump -Dstats.dump.component=app -Dstats.dump.overwrite -Dstats.output.dir=" + stats_dir + " -Dstats.output.filename=" + app + "-gpu.times " + java_opts
            os.putenv("JAVA_OPTS", opts)
            print "== executing application: " + app + " " + params[app],
            print "== with options: " + opts + "\n"
            ecode = os.system(props['delite.home'] + "/bin/exec " + app + ".deg " + params[app])
            if ecode != 0 and options['keep-going'] == None:
                print "Detected abnormal exit code, exiting"
                exit(-1)
 		



def isTflop():
    hostname = gethostname()
    if (hostname.startswith('tflop')):
        return True
    else:
        return False

def loadClasses(options):
    print """
=============================
==  Loading App Classes
============================="""
    f = open(props['delite.home'] + "/benchmark/config/classes", 'r')
    for line in f:
        tokens = line.split('|')
        if len(tokens) == 2:
            app = tokens.pop(0)
            clazz = tokens.pop(0)
            classes[app] = clazz.strip()
        else:
            print "ignoring[" + line + "] from class list"
    f.close()

def loadParams(options):
    if (isTflop()):
        hostname = 'tflop'
    else:
        hostname = 'default'
		
    if not 'datasets' in options:
      f = open(props['delite.home'] + '/benchmark/config/datasets.' + hostname + "." + options['input-size'], 'r')
    else:
      f = open(options['datasets'], 'r')
    for line in f:
        settings = line.strip().split('|')
        app = settings.pop(0)
        app_params = ''
        for param in settings:
            param = expand(param)
            app_params = app_params + ' ' + param
        params[app] = app_params.strip()
    f.close()
 
def expand(param):
    if (param[0] == '$'):
        return props['apps.data'] + "/" +  param[1:len(param)]
    else:
        return param   
    

if __name__ == "__main__":
    main()
