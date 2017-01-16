/* mk_unsigned.c --
       Create the test results for unsigned integers
*/

#include <stdio.h>
#include <stdlib.h>

int main( int argc, char *argv[] ) {

    FILE *infile;
    FILE *outfile;

    unsigned int a;
    unsigned int b;
    unsigned int c;
    char         operation[3];
    char         string[80];

    infile  = fopen( "unsigned.inp",  "r" );
    outfile = fopen( "unsigned.test", "w" );

    if ( infile == NULL ) {
        fprintf( stderr, "Could not open file \"unsigned.inp\" - program stopped\n" );
        exit( 1 );
    }

    while (1) {
        fgets( string, sizeof(string), infile );
        if ( feof( infile ) ) {
            break;
        }

        sscanf( string, "%u %s %u", &a, operation, &b );
        printf( "%u -- %s -- %u\n", a, operation, b );

        switch ( operation[0] ) {
            case '+':
                c = a + b;
                break;

            case '-':
                c = a - b;
                break;

            case '*':
                c = a * b;
                break;

            case '/':
                c = a / b;
                break;

            case '>':
                switch ( operation[1] ) {
                    case '\0':
                        c = a > b;
                        break;

                    case '=':
                        c = a >= b;
                        break;

                    case '>':
                        c = a >> b;
                        break;
                    default:
                        fprintf( stderr, "Unknown operation: %s\n", operation );
                        continue;
                }
                break;
            case '<':
                switch ( operation[1] ) {
                    case '\0':
                        c = a < b;
                        break;

                    case '=':
                        c = a <= b;
                        break;

                    case '<':
                        c = a << b;
                        break;

                    default:
                        fprintf( stderr, "Unknown operation: %s\n", operation );
                        continue;
                }
                break;

            default:
                fprintf( stderr, "Unknown operation: %s\n", operation );
                continue;
        }

        fprintf( outfile, "%u %s %u = %u\n", a, operation, b, c );
    }

    fclose( infile );
    fclose( outfile );
}
