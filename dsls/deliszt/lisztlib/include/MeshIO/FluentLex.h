#ifndef __FLUENT_LEX_H
#define __FLUENT_LEX_H
#include "MeshIO/Common.h"

namespace MeshIO {
enum TokenType { LPAREN, RPAREN, SYMBOL, STRING, DOUBLE, INT, NEWLINE, ERROR, EOF_TYPE };
const size_t BUF_SIZE = 4096;
struct Token {
	Token() : type(ERROR) {}
	TokenType type;
	union {
		char * s;
		unsigned int i;
		double d;
	} data;
};

static inline bool end_of_symbol(int s) {
	return s == EOF || s == ')' || s == '(' || isspace(s);
}
class FluentLex {
public:
	FluentLex() : newline_is_token(false), file(NULL) {}
	void init(const std::string & filename) {
		file = FOPEN(filename.c_str(), "r");
		if(!file) {
			fprintf(stderr,"error opening file: %s\n",filename.c_str());
			exit(1);
		}
	}
	//sometimes newlines have meanings in fluent, other times they do not
	void setNewlineSensitive(bool n) {
		newline_is_token = n;
	}
	void printToken(const Token & tok) {
		switch(tok.type) {
			case LPAREN:
				printf("(\n");
				break;
			case RPAREN:
				printf(")\n");
				break;
			case SYMBOL:
				printf("symbol: %s\n",tok.data.s);
				break;
			case STRING:
				printf("string: %s\n",tok.data.s);
				break;
			case DOUBLE:
				printf("double: %f\n",tok.data.d);
				break;
			case INT:
				printf("int: %d\n",tok.data.i);
				break;
			case NEWLINE:
				printf("\\n\n");
				break;
			case ERROR:
				printf("error: %s\n",tok.data.s);
				break;
			case EOF_TYPE:
				break;
		}
	}
	
	//TODO(zach): buffer will overrun on large tokens
	Token next() {
#define ERROR(msg) do { strncpy(buf,(msg),BUF_SIZE); goto error; } while(0)
		int c;
		Token tok;
		while( isspace(c = getc(file))) {
			if(c == '\n' && newline_is_token) {
				tok.type = NEWLINE;
				return tok;
			}
            if(c == EOF)
                break;
        }
		switch(c) {
			case '\n': break;
			case EOF:
				tok.type = EOF_TYPE;
				break;
			case '(':
				tok.type = LPAREN;
				break;
			case ')':
				tok.type = RPAREN;
				break;
			case '"': {
				int i = 0;
				while( (c = getc(file)) != '"') {
					switch(c) {
						case '\\':
							c = getc(file);
							if(c == EOF) {
								ERROR("unclosed string literal");
							} else {
								buf[i++] = c;
							}
							break;
						case EOF:
							ERROR("unclosed string literal");
							break;
						default:
							buf[i++] = c;
							break;
					}
				}
				buf[i] = '\0';
				tok.type = STRING;
				tok.data.s = buf;
				break;
			}
			default: {
				int i = 0;
				buf[i++] = c;
				while(!end_of_symbol(c = getc(file)))
					buf[i++] = c;
				buf[i] = '\0';
                ungetc(c,file);
				char * end;
				//is this an int?
				unsigned long v = strtoul(buf,&end,0x10); //every base is base 10!
				if(*end == '\0') {
					tok.type = INT;
					tok.data.i = v;
				} else {
					//what about a double?
					double vd = strtod(buf,&end);
					//printf("a double: \"%s\"\n",buf);
					if(*end == '\0') {
						tok.type = DOUBLE;
						tok.data.d = vd;
					} else {
						tok.type = SYMBOL;
						tok.data.s = buf;
					}
				}
				break;
			}
		}
		
		//printToken(tok);
		return tok;
#undef ERROR
error:
		tok.type = ERROR;
		tok.data.s = buf;
		return tok;
	}
	bool newline_is_token;
	char buf[BUF_SIZE];
	FILE * file;
};
} // namespace MeshIO
#endif
