#ifndef _FLUENT_PARSE_H
#define _FLUENT_PARSE_H

#include "MeshIO/FluentLex.h"
#include "MeshIO/LisztFileWriter.h"
#include "MeshIO/LisztFormat.h"
#include "MeshIO/EdgeTable.h"
#include "MeshIO/BoundaryTable.h"
#include <vector>
#include <string>

#define XF_COMMENT 0x0
#define XF_HEADER 0x1
#define XF_DIMENSION 0x2
#define XF_NODE 0x10
#define XF_CELL 0x12
#define XF_FACE 0x13
#define XF_ZONE_1 0x39
#define XF_ZONE_2 0x45

namespace MeshIO {
struct PhaseInfo {
	PhaseInfo() : seen_header(false), elem_size(0) {}
	bool seen_header;
	lsize_t elem_size;
};

class FluentParse {
private:
	FluentLex lex;
	int phase; //parsing positions, XF_NODE, XF_CELL, XF_FACE
	lsize_t last_elem_size;
	id_t last_elem_id; //id of expected next element to parse
	PhaseInfo node_phase, face_phase, cell_phase;
	
	//output stuff
	LisztHeader* header;
	EdgeTable edge_table;
	BoundaryTable* boundaries;

	LisztFileWriter* writer;
	
	bool expect(unsigned int i) {
		Token t = lex.next();
		return (t.type == INT && t.data.i == i);
	}
	bool expect(int i) {
		return expect((unsigned int) i);
	}
	bool expect(char c) {
		Token t = lex.next();
		if(c == '(')
			return t.type == LPAREN;
		else if(c == ')')
			return t.type == RPAREN;
		else
			return false;
	}
	bool expect(const char * sym) {
		Token t = lex.next();
		return strcmp(sym,t.data.s) == 0;
	}
	//someone at Fluent is on crack and made zone-ids be hexidecimal in one part of the file and decimal in the other
	//we have an int that was interpreted as a hex but was really decimal, lets fix that
	//TODO(zach): this breaks for really large decimal numbers (>99,999,999) that would overflow the hex but that is a lot of zones
	int interpret_hex_as_decimal(int a) {
		char buf[10];
		snprintf(buf,10,"%x",a);
		return atoi(buf);
	}
public:
	FluentParse() {}
	void init(LisztFileWriter* wr, const char* filename, const char* outfile) {
		lex.init(filename);
		phase = 0;
		last_elem_size = 0;
		last_elem_id = 0;
		writer = wr;
		header = &writer->header;
		boundaries = writer->boundaries;
		writer->init(outfile);
		
		header->nFE = 0;
		header->nBoundaries = 0;
	}
#define EXPECT(a) do { if(!expect( (a) ) ) { assert(!"unexpected token type"); } } while(0)
	unsigned int parseInt() {
		Token t = lex.next();
		assert(t.type == INT);
		return t.data.i;
	}
	double parseDouble() {
		Token t = lex.next();
		assert(t.type == DOUBLE);
		return t.data.d;
	}
	char * parseSymbol() {
		Token t = lex.next();
		assert(t.type == SYMBOL);
		return t.data.s;
	}
	char * parseString() {
		Token t = lex.next();
		assert(t.type == STRING);
		return t.data.s;
	}
	
	//change the phase to p, returns true if a phase transition occured, false if we are already in phase p
	bool setPhase(int p) {
		if(phase != p) {
			if(last_elem_id != last_elem_size) {
				printf("last set of element types was not finished; we don't support interleaving of fluent file sections (type = %x, expected = %d, found = %d",phase,last_elem_size,last_elem_id);
				assert(!"interleaved element types");
			}
			phase = p;
			last_elem_id = 0;
			PhaseInfo * info = NULL;
			switch(phase) {
				case XF_NODE:
					info = &node_phase;
					break;
				case XF_FACE:
					info = &face_phase;
					break;
				case XF_CELL:
					info = &cell_phase;
					break;
				default:
					printf("Unknown phase %d",phase);
					assert(!"unknown phase");
					break;
			}
			if(!info->seen_header) {
				printf("file contains elements before the header for those elements %d\n",phase);
				assert(!"data before header");
			}
			last_elem_size = info->elem_size;
			return true;
		} else {
			return false;
		}
	}
	
	void until(char c) {
		Token t;
		TokenType tt = c == ')' ? RPAREN : LPAREN;
		do {
			t = lex.next();
		} while(t.type != ERROR && t.type != EOF_TYPE && t.type != tt);
		assert(t.type == tt);
	}
	void parseAll() {
		writer->setBeginData();
		Token t; 
		while( (t = lex.next()).type != EOF_TYPE) {
			assert(t.type == LPAREN);
			int type = parseInt();
			switch(type) {
				case XF_COMMENT:
				case XF_HEADER:
					parseString();
					break;
				case XF_DIMENSION: {
					int dim = parseInt();
					if(dim != 3)
						assert(!"expected 3 dimensional mesh");
					} break;
				case XF_NODE: {
					EXPECT('(');
					unsigned int zone_id = parseInt();
					unsigned int first_index = parseInt();
					unsigned int last_index = parseInt();
                    int one = parseInt();
                    if(one != 1) {
                    	printf("WARNING: fluent says that node type should always be 1, here it is %d\n",one);
                    }
					int dim = parseInt();
					EXPECT(')');
					if(zone_id == 0) { //indicates header
						node_phase.seen_header = true;
						node_phase.elem_size = last_index;
						assert(first_index == 1);
						header->nV = last_index;
						if(dim != 3) {
							assert(!"expected 3 dimentional positions");
						}
					} else {
						if(setPhase(XF_NODE)) {
							//just changed to node phase, so we start the position table here
							header->position_table = writer->currentPosition();
						}
						EXPECT('(');
						assert(last_elem_id == first_index - 1 || !"elements not listed in order, this is not supported");
						last_elem_id = last_index;
						int nelems = last_index - first_index + 1;
						for(int i = 0; i < nelems; i++) {
							for(int j = 0; j < 3; j++) {
								writer->writeValue(parseDouble()); //write the doubles to the 
							}
						}
						EXPECT(')');
						
						boundaries->addBoundary(zone_id,VERTEX_T,first_index - 1,last_index);
					}
				} break;
				case XF_FACE: {
					EXPECT('(');
					
					unsigned int zone_id = parseInt();
					unsigned int first_index = parseInt();
					unsigned int last_index = parseInt();
					
					if(zone_id == 0) {
						/*int nfaces =*/ parseInt();
                        EXPECT(')');
                        face_phase.seen_header = true;
                        face_phase.elem_size = last_index;
                        header->nF = last_index;
					} else {
                        /*int bc_type =*/ parseInt();
                        int face_type = parseInt();
                        EXPECT(')');
                        
                        if(setPhase(XF_FACE)) {
                        	header->facet_edge_table = writer->currentPosition();
                        }
                        assert(last_elem_id == first_index - 1 || !"faces not listed in order, this is not supported");
                        boundaries->addBoundary(zone_id,FACE_T,first_index-1,last_index);
                        last_elem_id = last_index;
                        
                        //TODO(zach): check bc_type and face_type to ensure this isn't some special file
						EXPECT('(');
						lex.setNewlineSensitive(true);
						Token tok = lex.next();
						assert(tok.type == NEWLINE);
						std::vector<int> verts;
						for(id_t face_no = first_index - 1; face_no < last_index; face_no++) {
							if(face_type == 0) {//mixed faces so each is labeled with number of vertices
								/*int num_vertices =*/ parseInt();
							}
							//the cells come after the verts so we need two passes
							unsigned int num_vertices = 0;
							while((tok = lex.next()).type != NEWLINE) {
								assert(tok.type == INT);
								int id = tok.data.i; 
								if(num_vertices >= verts.size())
									verts.push_back(id);
								else
									verts[num_vertices] = id;
								num_vertices++;
							}
							int c0 = verts[num_vertices - 2]; //danger, make sure directions are not flipped, this is the standard place where the flipping usally occurs
							int c1 = verts[num_vertices - 1];
							num_vertices -= 2; //we over counted trying to find the newline, last 2 elements of verts are actually cells
							
							//fluent stores its faces as
							// N n0 n1 ... nN c0 c1
							// The fluent manual says that if you curl the fingers of your right hand in the direction that the nodes
							//are listed, your thumb will point to c1.  In the fluent files we have seen, this works
							//under the assumption that the coordinate system is left-handed i.e. x-axis cross y-axis = -z-axis,
							//for now Liszt is using a right-handed coordinate system, so your right hand should point to
							//c0 rather than c1.  Unlike the Joe framework, we make no guarentees about the 'outside' face being in the
							//second position
            
							//generate facet edges 
							for(unsigned int i = 0; i < num_vertices; i++) {
								FileFacetEdge fe;
								id_t v_from = verts[i] - 1; //nodes are 1-based in fluent
								id_t v_to = verts[(i+1) % num_vertices] - 1;
								id_t e = edge_table.get(v_from,v_to);
								
								fe.hf[0].vert = v_to;
								fe.hf[0].cell = c0;
								
								fe.hf[1].vert = v_from;
								fe.hf[1].cell = c1;
								
								fe.edge = e;
								fe.face = face_no;
								
								writer->writeObject(&fe);
								
								header->nFE++;
							}
						}
						lex.setNewlineSensitive(false);
						EXPECT(')');
					}
					} break;
				case XF_CELL: {
					EXPECT('(');
					unsigned int zone_id = parseInt();
					unsigned int first_index = parseInt();
					unsigned int last_index = parseInt();
                    
					if(zone_id == 0) {
						//do initialization stuff
						header->nC = last_index + 1; //0 is outside cell in fluent, so we have an extra cell and consider cells 0-indexed in the fluent file
						cell_phase.seen_header = true;
						cell_phase.elem_size = last_index;
                        EXPECT(0);
                        EXPECT(')');
                    } else {
                    	/*int type =*/ parseInt();
                        /*int element_type =*/ parseInt();
                        EXPECT(')');//add zone to stuff
                    	
                    	setPhase(XF_CELL);
                   
                    	assert(last_elem_id == first_index - 1 || !"cells not listed in order, this is not supported");
                    	last_elem_id = last_index;
						
						boundaries->addBoundary(zone_id,CELL_T,first_index, last_index);
					}
				} break;
				case XF_ZONE_1:
				case XF_ZONE_2: {
					EXPECT('(');
					int zone_id = interpret_hex_as_decimal(parseInt());
					char * zone_type = strdup(parseSymbol());
					char * zone_name = strdup(parseSymbol());
					//int domain_id = parseInt();
					EXPECT(')');
					
					EXPECT('(');
					EXPECT(')');
					
					boundaries->addBoundaryName(zone_name, zone_id);
					std::string bname(std::string("type:") + zone_type);
					boundaries->addBoundaryToAggregate(bname, zone_name);
					
					free(zone_name);
                    free(zone_type);
				} break;
				default:
					printf("Unknown top level type %x",type);
					assert(!"unhandled type");
					break;
			}
			EXPECT(')');
		}
		header->nE = edge_table.size();
	}
#undef EXPECT
};
} // namesapce MeshIO

#endif
