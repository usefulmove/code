#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cstring>

using namespace std;

/* program version */
static const char *PROGAM_VERSION = "0.0.3";

/* command syntax */
static const char *LIMITS_SYNTAX = "limits [OPTIONS]";

/* output control flags */
static bool debug = false;
static bool verbose = false;

/* program errors */
static int errors = 0;
static int fatal_errors = 0;

/* program controls */
static bool use_simple_set = false;
static bool remove_clearance_check = false;

class LimitTableElement {
    public:
        LimitTableElement();  // constructor
        int ID;
        bool original;
        double value;
    private:
};
LimitTableElement::LimitTableElement() {
    ID = 0;
    original = false;
    value = 0;
}

class SortedLimitTable {
    public:
        SortedLimitTable();  // constructor
        LimitTableElement elements[8];
        void add( double value, int ID, bool original);
        int CalculateValidShift();
        void ShiftCompleted( int shift);
        bool CheckClearance( int i, int j);
        int FindMate( int ind, int ID);
    private:
        int FULL_SIZE;
};
SortedLimitTable::SortedLimitTable() {
    FULL_SIZE = 8;
    //for (int n = 0; n < FULL_SIZE; ++n)
    //    elements[n] = new LimitTableElement;
}
void SortedLimitTable::add( double entry_value, int entry_ID, bool entry_original) {
    for (int n = 0; n < FULL_SIZE; ++n) {
        if (entry_value > elements[n].value) {
            for (int shift_index = (FULL_SIZE - 2); shift_index >= n; --shift_index) {
                elements[shift_index + 1].value = elements[shift_index].value;
                elements[shift_index + 1].ID = elements[shift_index].ID;
                elements[shift_index + 1].original = elements[shift_index].original;
            }
            elements[n].value = entry_value;
            elements[n].ID = entry_ID;
            elements[n].original = entry_original;
            return;
        }
    }
}
int SortedLimitTable::CalculateValidShift() {
    for (int n = 0; n < FULL_SIZE; ++n) {
        if (elements[n].original == true) {
            int m = FindMate( n, elements[n].ID);
            if (CheckClearance( n, m) && (m != -1))
                return elements[n].ID;
        }
    }
    return -1;  // no shift found
}
void SortedLimitTable::ShiftCompleted( int shift) {
    for (int n = 0; n < FULL_SIZE; ++n) {
        if (elements[n].ID == shift) {
            elements[n].original = false;
        }
    }
}
bool SortedLimitTable::CheckClearance( int i, int j) {
    if (remove_clearance_check)
        return true;

    if (i > j) {
        int temp = i;
        i = j;
        j = temp;
    }
    for (int index = i + 1; index < j; ++index) {
        if (elements[index].original == true)
            return false;
    }
    return true;
}
int SortedLimitTable::FindMate( int ind, int ID) {
    for (int n = 0; n < FULL_SIZE; ++n) {
        if ((elements[n].ID == ID) && (n != ind))
            return n;
    }
    return -1;  // no match
}

class LimitSet {
    public:
        LimitSet(); // constructor
        int Load( LimitSet set);
        double GetMinSoft();
        double GetMinHard();
        double GetMaxSoft();
        double GetMaxHard();
        void SetMinSoft( double value);
        void SetMinHard( double value);
        void SetMaxSoft( double value);
        void SetMaxHard( double value);
        void Generate();
        bool Valid();
        LimitSet Clone();
        void SetEqual( LimitSet set);
        bool Equals( LimitSet set);
        void Print();
    private:
        double max_hard;
        double max_soft;
        double min_hard;
        double min_soft;
};

LimitSet::LimitSet() {
    static bool seeded;

    if (!seeded) {
        time_t seconds;  // time variable used for seeding random number generator
        seconds = time(NULL);  // read current time
        srand(static_cast<unsigned int>(seconds));  // seed random number generator
        seeded = true;
    }

    /* generate random limit settings */
    min_hard = 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    min_soft = min_hard + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    max_soft = min_soft + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    max_hard = max_soft + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
}
int LimitSet::Load( LimitSet set) {
    LimitSet copy;
    copy = Clone();
    int failed;

    failed = 0;

    if (use_simple_set) {
        max_hard = set.GetMaxHard();
        failed += abs( static_cast<int>( !Valid()));
        max_soft = set.GetMaxSoft();
        failed += abs( static_cast<int>( !Valid()));
        min_hard = set.GetMinHard();
        failed += abs( static_cast<int>( !Valid()));
        min_soft = set.GetMinSoft();
        failed += abs( static_cast<int>( !Valid()));
    }
    else {
        SortedLimitTable sot;
        sot.add( max_hard, 3, true);
        sot.add( max_soft, 2, true);
        sot.add( min_hard, 1, true);
        sot.add( min_soft, 0, true);
        sot.add( set.GetMaxHard(), 3, false);
        sot.add( set.GetMaxSoft(), 2, false);
        sot.add( set.GetMinHard(), 1, false);
        sot.add( set.GetMinSoft(), 0, false);

        while (sot.CalculateValidShift() != -1) {
            int shift = sot.CalculateValidShift();

            switch (shift) {
                case 0:
                    min_soft = set.GetMinSoft();
                    break;
                case 1:
                    min_hard = set.GetMinHard();
                    break;
                case 2:
                    max_soft = set.GetMaxSoft();
                    break;
                case 3:
                    max_hard = set.GetMaxHard();
                    break;
                default:
                    fatal_errors++;
                    if (verbose)
                        std::cout << "fatal error: bad entry ID" << endl;
            }
            sot.ShiftCompleted( shift);

            failed += abs( static_cast<int>( !Valid()));
        }
    }

    if (failed) {
        if (verbose) {
            std::cout << endl << "orig - (" << copy.GetMinHard() << ", " << copy.GetMinSoft() << ", " << copy.GetMaxSoft() << ", " << copy.GetMaxHard() << ")" << endl;
            std::cout << "new - (" << set.GetMinHard() << ", " << set.GetMinSoft() << ", " << set.GetMaxSoft() << ", " << set.GetMaxHard() << ")" << endl << endl;
        }

        SetEqual( copy);  // reset original values
    }

    return (!static_cast<bool>(failed));
}
bool LimitSet::Valid() {
    if (min_hard > min_soft)
        return (false);
    if (min_soft > max_soft)
        return (false);
    if (max_soft > max_hard)
        return (false);

    return (true);
}
double LimitSet::GetMinSoft() {
    return (min_soft);
}
double LimitSet::GetMinHard() {
    return (min_hard);
}
double LimitSet::GetMaxSoft() {
    return (max_soft);
}
double LimitSet::GetMaxHard() {
    return (max_hard);
}
void LimitSet::SetMinSoft( double value) {
    min_soft = value;
}
void LimitSet::SetMinHard( double value) {
    min_hard = value;
}
void LimitSet::SetMaxSoft( double value) {
    max_soft = value;
}
void LimitSet::SetMaxHard( double value) {
    max_hard = value;
}
LimitSet LimitSet::Clone() {
    LimitSet clone;
    clone.SetMaxHard( GetMaxHard());
    clone.SetMaxSoft( GetMaxSoft());
    clone.SetMinHard( GetMinHard());
    clone.SetMinSoft( GetMinSoft());
    return (clone);
}
void LimitSet::SetEqual( LimitSet set) {
    max_hard = set.GetMaxHard();
    max_soft = set.GetMaxSoft();
    min_hard = set.GetMinHard();
    min_soft = set.GetMinSoft();
}
bool LimitSet::Equals( LimitSet set) {
    return (max_hard == set.GetMaxHard() && max_soft == set.GetMaxSoft() && min_hard == set.GetMinHard() && min_soft == GetMinSoft());
}
void LimitSet::Generate() {
    /* generate random limit settings */
    min_hard = 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    min_soft = min_hard + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    max_soft = min_soft + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
    max_hard = max_soft + 25.0*static_cast<double>(rand())/static_cast<double>(RAND_MAX);
}
void LimitSet::Print() {
    std::cout << "(" << min_hard << ", " << min_soft << ", " << max_soft << ", " << max_hard << ")" << endl;
}

void syntax_error() {
    std::cerr << "limits: incorrect syntax\n";
    std::cerr << "usage: " << LIMITS_SYNTAX << "\n";
    std::cerr << "type 'limits --help' for more information\n";
}

void invalid_option(char *option) {
    std::cerr << "limits: invalid option '" << option << "'\n";
    std::cerr << "type 'limits --help' for more information\n";
}


/***********************************************************************************
 * Procedure: main
 * Description: main program execution function
 * Arguments: argc -- number of program arguments (including program name)
 *            argv -- character pointer to argument list
 ***********************************************************************************/
int main( int argc, char *argv[]) {
    LimitSet currentLimitSettings;
    LimitSet newLimitSettings;
    int numberCycles = 1000000;  // total number of cycles (default)
    int cycles = 0;

    /* read options */
    while ((argc > 1) && (argv[1][0] == '-')) {
        switch (argv[1][1]) {
            case 'n':
                /* fall through */
            case 'N':
                /* number of iterations */
                sscanf(argv[2],"%d",&numberCycles);
                break;

            case 'v':
                /* fall through */
            case 'V':
                /* verbose mode */
                verbose = true;
                std::cout << "  (verbose mode on)\n";
                break;

            case 'D':
                /* debug mode */
                debug = true;
                std::cout << "  (debug mode on)\n";
                break;

            case '-':
                {
                    char *optname_ptr = &argv[1][2];

                    if (strcmp(optname_ptr, "help") == 0) {
                        /* help */
                        std::cout << "  usage: " << LIMITS_SYNTAX << "\n";
                        std::cout << "  options\n";
                        std::cout << "      -n  NUM         set number of cycles\n";
                        std::cout << "      -v, -verbose    enable verbose mode\n";
                        std::cout << "      -D              enable debug mode\n";
                        std::cout << "      --help          display help\n";
                        std::cout << "      --version       display version information\n";
                        std::cout << "      --simple-set    use simple limit set method\n";
                        std::cout << "      --remove-check  remove clearance check\n";
                        return (0);  // exit program
                    }
                    else if (strcmp(optname_ptr, "version") == 0) {
                        /* version */
                        std::cout << "  limits (version " << PROGAM_VERSION << ")\n";
                        return (0);
                    }
                    else if (strcmp(optname_ptr, "simple-set") == 0) {
                        /* simple set */
                        use_simple_set = true;
                        break;
                    }
                    else if (strcmp(optname_ptr, "remove-check") == 0) {
                        /* remove clearance check */
                        remove_clearance_check = true;
                        break;
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
                }  // end case '-':
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

        --argc;
        ++argv;
    }

    for (int n = 0; n < numberCycles; ++n) {
        if (!currentLimitSettings.Valid()) {
            fatal_errors++;
            if (verbose) {
                std::cout << "fatal error: invalid initial limits (" << n << ")" << endl;
                currentLimitSettings.Print();
            }
        }

        if (!newLimitSettings.Valid()) {
            fatal_errors++;
            if (verbose) {
                std::cout << "fatal error: invalid application limits (" << n << ")" << endl;
                newLimitSettings.Print();
            }
        }

        //std::cout << " *** current (before) *** " << endl;
        //currentLimitSettings.Print();
        //std::cout << " *** new *** " << endl;
        //newLimitSettings.Print();

        if (!currentLimitSettings.Load( newLimitSettings)) {
            errors++;
            if (verbose)
                std::cout << "load failure" << endl;
        }

        //std::cout << " *** current (after) *** " << endl;
        //currentLimitSettings.Print();

        if (!currentLimitSettings.Valid()) {
            fatal_errors++;
            if (verbose) {
                std::cout << "fatal error: invalid limits after load call (" << n << ")" << endl;
                currentLimitSettings.Print();
            }
        }

        newLimitSettings.Generate();  // generate new limit settings

        cycles++;
    }

    std::cout << "completed with " << errors << " errors out of " << cycles << " cycles" << endl;
    if (fatal_errors)
        std::cout << "fatal errors: " << fatal_errors << endl;

    return (0);
}
