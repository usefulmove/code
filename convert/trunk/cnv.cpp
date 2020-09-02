/******************************************************************************
 * File: cnv.cpp
 * Description: This program is a conversion utility that is used to convert
 *       (or clean) a single text file from one file format (line ending
 *       convention) to another.
 * Usage: cnv [options] <input_file> [<output_file>]
 * Arguments: <input_file>  -- path and filename for file to be converted
 *            <output_file> -- path and filename for converted file
 * Options: -u, --unix       convert to unix format (LF) [default]
 *          -d, --dos        convert to win/dos format (CR + LF)
 *          -m, --mac        convert to mac osx format (CR)
 *          -c, --clean      unix clean (remove each CR found in file)
 *          -n, --copy-only  do not convert file format, just copy file
 *          -v, --verbose    verbose mode
 *          --help,          display help
 *          --version,       display version information
 * Created: August 2, 2007 rde
 * Updated: September 22, 2007 rde
 ******************************************************************************/

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <memory>


/* program version */
static const char *VERSION = "0.0.9";

/* command syntax */
static const char *CNV_SYNTAX = "cnv [OPTIONS] <input> [<output>]";

/* character constants */
static const char CR = 13;
static const char LF = 10;

/* verbose flag */
static bool verbose = false;

/******************************************************************************
 * CConverter class
 ******************************************************************************/
class CConverter {
    protected:
        std::ifstream in;
        std::ofstream out;
    public:
        virtual int c_init(const char *i_filename, const char *o_filename);
        virtual ~CConverter();
        virtual void convert() = 0;
};
/* destructor */
CConverter::~CConverter() {
    /* close files */
    in.close();
    out.close();
}
/* c_init method */
int CConverter::c_init(const char *i_filename, const char *o_filename) {
    /* open input file */
    if (verbose)
        std::cout << "opening input file - " << i_filename << "\n";
    in.open(i_filename);
    if (in.bad()) {
        std::perror(i_filename);
        return EXIT_FAILURE;
    }

    /* open output file */
    out.open(o_filename);
    if (verbose)
        std::cout << "opening output file - " << o_filename << "\n";
    if (out.bad()) {
        std::perror(o_filename);
        return EXIT_FAILURE;
    }

    return (0);
}


/******************************************************************************
 * CConverter_Unix class (derived from abstract CConverter class)
 ******************************************************************************/
class CConverter_Unix : public CConverter {
    public:
        void convert();
};
void CConverter_Unix::convert() {
    char chr;

    if (verbose)
        std::cout << "converting input file to unix format...\n";

    while (in.get(chr)) {
        switch (chr) {
            case CR:
                in.get(chr);
                switch (chr) {
                    case LF:
                        out.put(LF);
                        break;
                    case CR:
                        out.put(LF);
                        out.put(LF);
                        break;
                    default:
                        out.put(LF);
                        out.put(chr);
                        break;
                }
                break;
            default:
                out.put(chr);
                break;
        }
    }

    if (verbose)
        std::cout << "completed unix conversion\n";
}


/******************************************************************************
 * CConverter_Win class (derived from abstract CConverter class)
 ******************************************************************************/
class CConverter_Win : public CConverter {
    public:
        void convert();
};
void CConverter_Win::convert() {
    char chr;

    if (verbose)
        std::cout << "converting input file to dos format...\n";

    while (in.get(chr)) {
        switch (chr) {
            case CR:
                in.get(chr);
                switch (chr) {
                    case LF:
                        out.put(CR);
                        out.put(LF);
                        break;
                    case CR:
                        out.put(CR);
                        out.put(LF);
                        out.put(CR);
                        out.put(LF);
                        break;
                    default:
                        out.put(CR);
                        out.put(LF);
                        out.put(chr);
                        break;
                }
                break;
            case LF:
                out.put(CR);
                out.put(LF);
                break;
            default:
                out.put(chr);
                break;
        }
    }

    if (verbose)
        std::cout << "completed dos conversion\n";
}


/******************************************************************************
 * CConverter_OSX class (derived from abstract CConverter class)
 ******************************************************************************/
class CConverter_OSX : public CConverter {
    public:
        void convert();
};
void CConverter_OSX::convert() {
    char chr;

    if (verbose)
        std::cout << "converting input file to mac osx format...\n";

    while (in.get(chr)) {
        switch (chr) {
            case CR:
                in.get(chr);
                switch (chr) {
                    case LF:
                        out.put(CR);
                        break;
                    default:
                        out.put(CR);
                        out.put(chr);
                        break;
                }
                break;
            case LF:
                out.put(CR);
                break;
            default:
                out.put(chr);
                break;
        }
    }

    if (verbose)
        std::cout << "completed mac osx conversion\n";
}


/******************************************************************************
 * CConverter_UnixClean class (derived from abstract CConverter class)
 ******************************************************************************/
class CConverter_UnixClean : public CConverter {
    public:
        void convert();
};
void CConverter_UnixClean::convert() {
    char chr;

    if (verbose)
        std::cout << "cleaning unix file...\n";

    while (in.get(chr)) {
       if (chr != CR)
            out.put(chr);
    }

    if (verbose)
        std::cout << "completed unix file clean\n";
}

void syntax_error() {
    std::cerr << "cnv: incorrect syntax\n";
    std::cerr << "usage: " << CNV_SYNTAX << "\n";
    std::cerr << "type 'cnv --help' for more information\n";
}

void invalid_option(char *option) {
    std::cerr << "cnv: invalid option '" << option << "'\n";
    std::cerr << "type 'cnv --help' for more information\n";
}


/******************************************************************************
 * CConverter_Copy class (derived from abstract CConverter class)
 ******************************************************************************/
class CConverter_Copy : public CConverter {
    public:
        int c_init(const char *i_filename, const char *o_filename);
        void convert();
};
/* c_init method */
int CConverter_Copy::c_init(const char *i_filename, const char *o_filename) {
    /* open input file (binary) */
    if (verbose)
        std::cout << "opening input file - " << i_filename << "\n";
    in.open(i_filename, std::ios::in | std::ios::binary);
    if (in.bad()) {
        std::perror(i_filename);
        return EXIT_FAILURE;
    }

    /* open output file (binary) */
    out.open(o_filename, std::ios::out | std::ios::binary);
    if (verbose)
        std::cout << "opening output file - " << o_filename << "\n";
    if (out.bad()) {
        std::perror(o_filename);
        return EXIT_FAILURE;
    }

    return (0);
}
/* convert method */
void CConverter_Copy::convert() {
    if (verbose)
        std::cout << "copying input file...\n";

    out << in.rdbuf();  // direct input stream buffer to output stream

    if (verbose)
        std::cout << "completed file copy\n";
}


/******************************************************************************
 * main
 ******************************************************************************/
int main(int argc, char *argv[]) {
    char *default_output = "out";
    bool overwrite = false;
    CConverter *ptr = NULL;
    std::auto_ptr<CConverter> conv_ptr(ptr);


    if (argc < 2) {
        syntax_error();
        return EXIT_FAILURE;
    }

    /* read options */
    while ((argc > 1) && (argv[1][0] == '-')) {
        /* option has been entered */
        if (argv[1][1] == '-') {
            /* expanded option format */

            char *optname_ptr = &argv[1][2];

            if (strcmp(optname_ptr, "unix") == 0) {
                if (conv_ptr.get() == NULL)
                    conv_ptr.reset(new CConverter_Unix);
                else {
                    std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                    std::cerr << "type 'cnv --help' for more information\n";
                    return EXIT_FAILURE;
                }
            }
            else if (strcmp(optname_ptr, "dos") == 0) {
                if (conv_ptr.get() == NULL)
                    conv_ptr.reset(new CConverter_Win);
                else {
                    std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                    std::cerr << "type 'cnv --help' for more information\n";
                    return EXIT_FAILURE;
                }
            }
            else if (strcmp(optname_ptr, "mac") == 0) {
                if (conv_ptr.get() == NULL)
                    conv_ptr.reset(new CConverter_OSX);
                else {
                    std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                    std::cerr << "type 'cnv --help' for more information\n";
                    return EXIT_FAILURE;
                }
            }
            else if (strcmp(optname_ptr, "clean") == 0) {
                if (conv_ptr.get() == NULL)
                    conv_ptr.reset(new CConverter_UnixClean);
                else {
                    std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                    std::cerr << "type 'cnv --help' for more information\n";
                    return EXIT_FAILURE;
                }
            }
            else if (strcmp(optname_ptr, "copy-only") == 0) {
                if (conv_ptr.get() == NULL)
                    conv_ptr.reset(new CConverter_Copy);
                else {
                    std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                    std::cerr << "type 'cnv --help' for more information\n";
                    return EXIT_FAILURE;
                }
            }
            else if (strcmp(optname_ptr, "verbose") == 0) {
                verbose = true;  // verbose -- set verbose flag to true
            }
            else if (strcmp(optname_ptr, "help") == 0) {
                std::cout << "  usage: " << CNV_SYNTAX << "\n";
                std::cout << "  options\n";
                std::cout << "      -u, --unix       convert to unix format (LF) [default]\n";
                std::cout << "      -d, --dos        convert to win/dos format (CR + LF)\n";
                std::cout << "      -m, --mac        convert to mac osx format (CR)\n";
                std::cout << "      -c, --clean      unix clean (remove all CR found in file)\n";
                std::cout << "      -n, --copy-only  do not convert file format, copy only\n";
                std::cout << "      -v, --verbose    verbose mode\n";
                std::cout << "      --help           display help\n";
                std::cout << "      --version        display version information\n";
                return (0);  // exit program
            }
            else if (strcmp(optname_ptr, "version") == 0) {
                std::cout << "  cnv (version " << VERSION << ")\n\n";
                std::cout << "  Copyright (c) 2007 Robert Duane Edmonds\n";
                return (0);  // exit program
            }
            else {
                char bad_option[255] = {'\0'};
                bad_option[0] = '-';
                bad_option[1] = '-';
                char *src_ptr = optname_ptr;
                int index = 2;
                while ((*src_ptr) != '\0') {
                    bad_option[index] = (*src_ptr);
                    ++src_ptr;
                    ++index;
                }
                invalid_option(bad_option);
                return EXIT_FAILURE;
            }
        }
        else {
            /* common option format */

            char *opt_ptr = &(argv[1][1]);

            while ((*opt_ptr) != '\0') {
                switch ((*opt_ptr)) {
                    case 'd':
                        /* fall through */
                    case 'D':
                        if (conv_ptr.get() == NULL)
                            conv_ptr.reset(new CConverter_Win);
                        else {
                            std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                            std::cerr << "type 'cnv --help' for more information\n";
                            return EXIT_FAILURE;
                        }
                        break;

                    case 'm':
                        /* fall through */
                    case 'M':
                        if (conv_ptr.get() == NULL)
                            conv_ptr.reset(new CConverter_OSX);
                        else {
                            std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                            std::cerr << "type 'cnv --help' for more information\n";
                            return EXIT_FAILURE;
                        }
                        break;

                    case 'c':
                        /* fall through */
                    case 'C':
                        if (conv_ptr.get() == NULL)
                            conv_ptr.reset(new CConverter_UnixClean);
                        else {
                            std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                            std::cerr << "type 'cnv --help' for more information\n";
                            return EXIT_FAILURE;
                        }
                        break;

                    case 'n':
                        /* fall through */
                    case 'N':
                        if (conv_ptr.get() == NULL)
                            conv_ptr.reset(new CConverter_Copy);
                        else {
                            std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                            std::cerr << "type 'cnv --help' for more information\n";
                            return EXIT_FAILURE;
                        }
                        break;

                    case 'u':
                        /* fall through */
                    case 'U':
                        if (conv_ptr.get() == NULL)
                            conv_ptr.reset(new CConverter_Unix);
                        else {
                            std::cerr << "cnv: conflicting or redundant conversion types specified\n";
                            std::cerr << "type 'cnv --help' for more information\n";
                            return EXIT_FAILURE;
                        }
                        break;

                    case 'v':
                        /* fall through */
                    case 'V':
                        verbose = true;  // verbose -- set verbose flag to true
                        break;

                    default:
                        char bad_option[255] = {'\0'};
                        bad_option[0] = '-';
                        char *src_ptr = &argv[1][1];
                        int index = 1;
                        while ((*src_ptr) != '\0') {
                            bad_option[index] = (*src_ptr);
                            ++src_ptr;
                            ++index;
                        }
                        invalid_option(bad_option);
                        return EXIT_FAILURE;
                }
                ++opt_ptr;
            }
        }
        --argc;
        ++argv;
    }

    // if no options have been set to specify conversion type, perform unix conversion
    if (conv_ptr.get() == NULL)
        conv_ptr.reset(new CConverter_Unix);

    /* input and output file arguments */
    char *input_ptr;  // pointer to input filename
    char *output_ptr;  // pointer to output filename
    if (argc > 2) {
        input_ptr  = argv[1];
        output_ptr = argv[2];
    }
    else if (argc == 2) {
        input_ptr  = argv[1];
        output_ptr = default_output;
        overwrite = true;
    }
    else {
        /* error condition -- not enough arguments */
        syntax_error();
        return EXIT_FAILURE;
    }

    /* convert file */
    conv_ptr->c_init(input_ptr, output_ptr);
    conv_ptr->convert();

    /* copy temporary file back to original */
    if (overwrite) {
        conv_ptr.reset(new CConverter_Copy);
        char *copy_ptr;
        copy_ptr = input_ptr;
        input_ptr = output_ptr;
        output_ptr = copy_ptr;

        conv_ptr->c_init(input_ptr, output_ptr);
        conv_ptr->convert();

        /* remove temporary file */
        remove(input_ptr);
    }

    return (0);
}
