
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ext/hash_map>
#include "MeshIO/LisztFormat.h"
#include "MeshIO/LisztFileWriter.h"

#define MAX_LINE_SIZE 2048

namespace MeshIO {

template<typename T>
struct StructHash {
	size_t operator()(const T & data) const {
		const int * key = (int *) &data;
		size_t len = sizeof(T) / sizeof(int);
		size_t h;
		size_t i;
		//http://burtleburtle.net/bob/hash/hashfaq.html
		for (h=0, i=0; i<len; ++i) { h += key[i]; h += (h<<10); h ^= (h>>6); } 
		h += (h<<3); h ^= (h>>11); h += (h<<15); 
		return h;
	}
	bool operator()(const T & lhs, const T & rhs) const {
		int * l = (int *) &lhs;
		int * r = (int *) &rhs;
		for(size_t i = 0; i < sizeof(T)/sizeof(int); i++)
			if(l[i] != r[i])
				return false;
		return true;
	}
};

class DPLParse {
public:
	enum { BOUNDARY_PASS, TOPOLOGY_PASS } pass;
	#define BOUNDARY_ONLY if(pass != BOUNDARY_PASS) return
	#define TOPOLOGY_ONLY if(pass != TOPOLOGY_PASS) return
	
	DPLParse() : next_face_id(0), next_edge_id(0), next_fe_id(0) {}
	struct Triangle { id_t a; id_t b; id_t c; };
	struct Edge { id_t head; id_t tail; };
	struct FE { id_t face; id_t edge; };
	
	__gnu_cxx::hash_map<Triangle,id_t,StructHash<Triangle>,StructHash<Triangle> > triangle_map;
	size_t next_face_id;
	__gnu_cxx::hash_map<Edge,id_t,StructHash<Edge>,StructHash<Edge> > edge_map;
	size_t next_edge_id;
	__gnu_cxx::hash_map<FE,id_t,StructHash<FE>,StructHash<FE> > fe_map;
	size_t next_fe_id;
	std::vector<FileFacetEdge> ffes;
	
	LisztHeader * header;
	LisztFileWriter * writer;
	const char *  filename;
	
	void init(LisztFileWriter * wr, const char * infile, const std::string & outfile) {
		writer = wr;
		header = &writer->header;
		writer->init(outfile);
		filename = infile;
	}
	void parseAll() {
	
		printf("beginning boundaries\n");
		pass = BOUNDARY_PASS;
		writer->setBeginData();
		parseFile(filename);
		printf("beginning topology\n");
		pass = TOPOLOGY_PASS;
		parseFile(filename);
		printf("ending topology\n");
	}
	
	id_t face(id_t a, id_t b, id_t c) {
		if(a > b)
			std::swap(a,b);
		if(a > c)
			std::swap(a,c);
		if(b > c)
			std::swap(b,c);
		Triangle tri = {a,b,c};
		if(triangle_map.count(tri) == 0) {
			id_t id = next_face_id++;
			triangle_map[tri] = id;
			return id;
		} else {
			return triangle_map[tri];
		}
	}
	id_t edge(id_t a, id_t b) {
		if(a > b)
			std::swap(a,b);
		Edge edge = { a, b };
		if(edge_map.count(edge) == 0) {
			id_t id = next_edge_id++;
			edge_map[edge] = id;
			return id;
		} else {
			return edge_map[edge];
		}
	}
	bool facet_edge(id_t f, id_t e, FileFacetEdge ** ffe) {
		FE fe = {f , e};
		if(fe_map.count(fe) == 0) {
			id_t id = next_fe_id++;
			FileFacetEdge temp;
			memset(&temp,0,sizeof(FileFacetEdge));
			ffes.push_back(temp);
			*ffe = &ffes.back();
			fe_map[fe] = id;
			return true;
		} else {
			*ffe = &ffes[fe_map[fe]];
			return false;
		}
	}

	void dimentions(int dim) {
		if(dim != 3) {
			fprintf(stderr,"incorrect dim = %d\n",dim);
			return;
		}
	}
	void beginElems(int elem) {
		TOPOLOGY_ONLY;
		header->nC = elem + 1;
	}
	void tet(int a, int b, int c, int d, int id) {
		TOPOLOGY_ONLY;
		id_t verts[4] = {a,b,c,d};
		id_t cell = id + 1;
		int faces[][3] = { {0,1,2}, {1,3,2}, {1,0,3}, {0,2,3} };
		for(int i = 0; i < 4; i++) {
			int * fidx = faces[i];
			id_t f = face( verts[ fidx[0]], verts[fidx[1]], verts[fidx[2]] );
			for(int j = 0; j < 3; j++) {
				id_t head = verts[fidx[(j+1)%3]];
				id_t tail = verts[fidx[j]];
				id_t e = edge(tail,head);
				FileFacetEdge * ffe;
				if(facet_edge(f,e,&ffe)) {
					ffe->face = f;
					ffe->edge = e;
					ffe->hf[0].vert = head;
					ffe->hf[0].cell = cell;
					ffe->hf[1].vert = tail;
					//ffe->hf[0].cell = set when we see the other size of this facet edge
				} else {
					ffe->hf[1].cell = cell;
				}
			}
		}
	}
	void endElems() {
		TOPOLOGY_ONLY;
		header->nF = triangle_map.size();
		header->nE = edge_map.size();
		header->nFE = ffes.size();
		header->facet_edge_table = writer->currentPosition();
		for(size_t i = 0; i < ffes.size(); i++) {
			writer->writeObject(&ffes[i]);
		}
	}
	
	void beginPoints(int npoints) {
		TOPOLOGY_ONLY;
		header->nV = npoints;
		header->position_table = writer->currentPosition();
	}
	void point(double a, double b, double c, int id) {
		TOPOLOGY_ONLY;
		writer->writeValue(a);
		writer->writeValue(b);
		writer->writeValue(c);
	}
	void endPoints() {}
	
	
	id_t boundary_start;
	char boundary_name[32];
	void numBoundaries(int num) {}
	void beginBoundary(int mark) {
		BOUNDARY_ONLY;
		boundary_start = next_face_id;
		sprintf(boundary_name,"%d",mark);
	}
	void boundaryElems(int nelems) {}
	void triangle(int a, int b, int c) {
		BOUNDARY_ONLY;
		face(a,b,c); //we touch this face before running topology to ensure it gets a sequential id in the boundary set
	}
	void endBoundary() {
		BOUNDARY_ONLY;
		writer->boundaries->addBoundary(boundary_name, MeshIO::FACE_T, boundary_start,next_face_id);
	}
	
	bool startsWith(const char * prefix, char * string) {
		int len = strlen(prefix);
		return strncmp(prefix,string,len) == 0;
	}
	void parseFile(const char * name) {
		FILE * file = fopen(name,"r");
		if(file == NULL) {
			fprintf(stderr,"could not open file: %s\n",name);
			return;
		}
		char buf[MAX_LINE_SIZE];
		#define SAFE_READ_LINE(r) \
		do { \
			char * result = fgets(r,MAX_LINE_SIZE,file); \
			if(result == NULL) { \
				fprintf(stderr,"failed to read line\n"); \
				return; \
			} \
		} while(0);
		while(NULL != fgets(buf,MAX_LINE_SIZE,file)) {
			if(startsWith("%",buf))
				continue;
			else if(startsWith("NDIME=",buf)) {
				dimentions(atoi(&buf[6]));
			}
			else if(startsWith("NELEM=",buf)) {
				int n = atoi(&buf[6]);
				beginElems(n);
				for(int i = 0; i < n; i++) {
					SAFE_READ_LINE(buf);
					int tid = atoi(strtok(buf,"\t"));
					if(tid != 10) {
						fprintf(stderr,"not a tet: %d\n",tid);
						return;
					}
					int a = atoi(strtok(NULL,"\t"));
					int b = atoi(strtok(NULL,"\t"));
					int c = atoi(strtok(NULL,"\t"));
					int d = atoi(strtok(NULL,"\t"));
					int id = atoi(strtok(NULL,"\t"));
					tet(a,b,c,d,id);
				}
				endElems();
			}
			else if(startsWith("NPOIN=",buf)) {
				int n = atoi(&buf[6]);
				beginPoints(n);
				for(int i = 0; i < n; i++) {
					SAFE_READ_LINE(buf);
					double v0 = atof(strtok(buf,"\t"));
					double v1 = atof(strtok(NULL,"\t"));
					double v2 = atof(strtok(NULL,"\t"));
					int id = atoi(strtok(NULL,"\t"));
					point(v0,v1,v2,id);
				}
				endPoints();
			}
			else if(startsWith("NMARK=",buf)) {
				numBoundaries(atoi(&buf[6]));
			}
			else if(startsWith("MARKER_INDEX=",buf)) {
				beginBoundary(atoi(&buf[13]));
			}
			else if(startsWith("MARKER_ELEMS=",buf)) {
				int n = atoi(&buf[13]);
				boundaryElems(n);
				for(int i = 0; i < n; i++) {
					SAFE_READ_LINE(buf);
					int tri = atoi(strtok(buf,"\t"));
					if(tri != 5) {
						fprintf(stderr,"not a tri %d\n",tri);
						return;
					}
					int a = atoi(strtok(NULL,"\t"));
					int b = atoi(strtok(NULL,"\t"));
					int c = atoi(strtok(NULL,"\t"));
					triangle(a,b,c);
				}
				endBoundary();
			} else {
				fprintf(stderr,"skipping non-recongized token %s",buf);
			}
		}
		printf("ending pass\n");
		fclose(file);
	}
};
} //namespace MeshIO