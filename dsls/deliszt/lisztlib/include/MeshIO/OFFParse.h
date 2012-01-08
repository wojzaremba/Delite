#ifndef _OFF_PARSE_H
#define _OFF_PARSE_H

#include <vector>
#include "MeshIO/LisztFileWriter.h"
#include "MeshIO/LisztFormat.h"
#include "MeshIO/EdgeTable.h"

namespace MeshIO {
class OFFParse {
private:
	//output stuff
	LisztHeader* header;
	EdgeTable edge_table;
	LisztFileWriter* writer;
	
public:
	OFFParse() {}
	void init(LisztFileWriter* wr, const std::string & filename,
			const std::string & outfile) {
		writer = wr;
		header = &writer->header;
		file = FOPEN(filename.c_str(),"r");
		if(!file) {
			perror(NULL);
			assert(!"could not read off file");
		}
		writer->init(outfile);
		header->nV = 0;
		header->nE = 0;
		header->nF = 0;
		header->nC = 0;
		header->nFE = 0;
	}
	void parseAll() {
		writer->setBeginData();
		
		
		char * line = readline();
		int ejunk;
		sscanf(line,"%d %d %d",&header->nV,&header->nF,&ejunk);
		//positions
		header->position_table = writer->currentPosition();
		for(unsigned int i = 0; i < header->nV; i++) {
			line = readline();
			double p[3];
			sscanf(line,"%lf %lf %lf",p,p+1,p+2);
			for(int j = 0; j < 3; j++)
				writer->writeValue(p[j]);
		}
		header->facet_edge_table = writer->currentPosition();
		for(unsigned int i = 0; i < header->nF; i++) {
			line = readline();
			std::vector<id_t> verts;
			int nedges;
			int pos;
			sscanf(line,"%d%n",&nedges,&pos);
			verts.reserve(nedges);
			for(int j = 0; j < nedges; j++) {
				line += pos;
				int v;
				sscanf(line,"%d%n",&v,&pos);
				verts.push_back(v);
			}
			for(int j = 0; j < nedges; j++) {
				id_t tail = verts[j];
				id_t head = verts[(j+1)%nedges];
				id_t edge = edge_table.get(tail,head);
				addFacetEdge(i,edge,head,0,tail,1);
			}
		}
		//header
		header->nE = edge_table.size();
		header->nC = 2;
	}
private:
	char * readline() {
		assert(file);
		char * line;
		do {
			line = fgets(buf,2048,file);
			if(!line) {
				perror(NULL);
				assert(!"read failed");
			}
			while(isspace(*line)) { line++; }
		} while(line[0] == '#' || strncmp(line,"OFF",3) == 0);
		char * cur = line;
		while(*cur && *cur != '#')
			cur++;
		*cur = '\0';
		return line;
	}
	void addFacetEdge(id_t face, id_t edge, id_t vert0, id_t cell0, id_t vert1, id_t cell1) {
		FileFacetEdge fe;
		fe.face = face;
		fe.edge = edge;
		fe.hf[0].vert = vert0;
		fe.hf[0].cell = cell0;
		fe.hf[1].vert = vert1;
		fe.hf[1].cell = cell1;
		printf("fe = %d, f = %d, e = %d,  (v = %d, c = %d) (v = %d, c = %d)\n",
		header->nFE,fe.face,fe.edge, fe.hf[0].vert, fe.hf[0].cell, fe.hf[1].vert, fe.hf[1].cell);
	
		writer->writeObject(&fe);
		header->nFE++;
	}
	char buf[2048];
	FILE * file;
};
} // namespace MeshIO
#endif /* _OFF_PARSE_H */
