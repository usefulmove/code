/***********************************************************************************
 * Program: cointoss
 * Filename: cointoss.cpp
 * Description: This program...
 * Usage: cointoss [options]
 * Options: -n, number of coin flips to analyze
 * Created: 16 Aug 2007 (bb) rde
 * Updated: 26 May 2012 rde
 ***********************************************************************************/

#include <iostream>
#include <cstdlib>
#include <ctime>
#include <deque>
#include <cstring>
#include <cstdio>

using namespace std;

/* program version */
static const char *PROGAM_VERSION = "0.8.7";

/* command syntax */
static const char *COINTOSS_SYNTAX = "cointoss [OPTIONS]";

/* output control flags */
static bool debug = false;
static bool verbose = false;


/***********************************************************************************
 * Class: Coin (two-sided)
 ***********************************************************************************/
class Coin {
    public:
        static const bool HEADS = true;
        static const bool TAILS = false;
        void Flip();
        bool GetSide();
        Coin(); // constructor
        // use default destructor
    private:
        bool side;
};
/***********************************************************************************
 * Method: flip
 * Description: This method is used to simulate the flipping of the coin such that
 *       the (visible) side result is random.
 * Usage: coin_obj.Flip()
 * Arguments: none
 * Return: none
 ***********************************************************************************/
void Coin::Flip() {
    if ((rand() % 2) == 0)
        side = HEADS;
    else
        side = TAILS;
}
/***********************************************************************************
 * Method: getside
 * Description: This method is used to access the (visible) side of the coin object.
 * Usage: coin_obj.GetSide()
 * Arguments: none
 * Return: side (HEADS or TAILS)
 ***********************************************************************************/
bool Coin::GetSide() {
    return side;
}
/***********************************************************************************
 * Constructor
 ***********************************************************************************/
Coin::Coin() {
    time_t seconds;  // time variable used for seeding random number generator
    seconds = time( NULL);  // read current time
    srand( static_cast<unsigned int>( seconds));  // seed random number generator

    side = HEADS;
}


/***********************************************************************************
 * Class: Cointosser
 ***********************************************************************************/
class Cointosser {
    public:
        static const int PATTERNLENGTH = 3;
        void Toss();
        void TossAndCheck();
        void SetPattern( bool match_sequence[]);  // set match pattern
        void GenerateResults();
        long int GetHeads();
        long int GetTails();
        long int GetMatches();
        Cointosser();  // constructor
        // use default destructor
    private:
        bool match_pattern[PATTERNLENGTH];  // match pattern
        Coin test_coin;  // coin
        deque<bool> cresults;  // results data
        int match_counter;
        long int match_count_sum;  // match count total
        long int matches;  // number of matches
        long int number_heads;  // number of heads
        long int number_tails;  // number of tails
};
/***********************************************************************************
 * Method: Toss
 * Description: This method is used to cause the cointosser to flip a coin.
 * Usage: cointosser_obj.Toss()
 * Arguments: none
 * Return: none
 ***********************************************************************************/
void Cointosser::Toss() {
    test_coin.Flip();  // flip coin

    /* record result */
    cresults.push_back( test_coin.GetSide());
    if (cresults.size() > 3) {
        cresults.pop_front();  // remove front node
    }

    if (test_coin.GetSide() == test_coin.HEADS)
        ++number_heads;
    else
        ++number_tails;
    ++match_counter;
}
/***********************************************************************************
 * Method: TossAndCheck
 * Description: This method is used to cause the cointosser to flip a coin and check
 *       the result history for a match.
 * Usage: cointosser_obj.TossAndCheck()
 * Arguments: none
 * Return: none
 ***********************************************************************************/
void Cointosser::TossAndCheck() {
    /* flip coin and record result */
    Toss();

    /* check for pattern match */
    int array_index = 0;  //
    deque<bool>::iterator iter;  // iterator
    for (iter = cresults.begin(); iter != cresults.end(); ++iter) {
        if ((*iter) != match_pattern[array_index]) {
            return;  // no match
        }
        ++array_index;  // increment array index
    }

    if (match_counter < 3)
        return;  /* These matches have to be rejected because the test is for number
                    of coin tosses required to match the pattern. If these are not
                    rejected, the program will falsely identify matches based on
                    coin tosses from previous trials. */

    /* match */
    ++matches;
    match_count_sum += match_counter;
    match_counter = 0;  // reset match counter
    if (debug)
        std::cout << "  pattern match\n";
}
/***********************************************************************************
 * Method: SetPattern
 * Description: This method is used to set the match pattern.
 * Usage: cointosser_obj.SetPattern( match_sequence)
 * Arguments: match_sequence -- array containing sequence that identifies a match
 * Return: none
 ***********************************************************************************/
void Cointosser::SetPattern( bool match_sequence[]) {
    for (int ind = 0; ind < PATTERNLENGTH; ++ind)
        match_pattern[ind] = match_sequence[ind];
}
/***********************************************************************************
 * Method: GenerateResults
 * Description: This method is used to calculate coin toss pattern match results and
 *       display them.
 * Usage: cointosser_obj.GenerateResults()
 * Arguments: match_sequence -- array containing sequence that identifies a match
 * Return: none
 ***********************************************************************************/
void Cointosser::GenerateResults() {
    double srchCountAvg;

    if (matches > 0)
        srchCountAvg = static_cast<double>( match_count_sum)/static_cast<double>( matches);
    else
        srchCountAvg = 0;

    std::cout << "  average search for ";
    for (int count = 0; count < PATTERNLENGTH; ++count)
        std::cout << match_pattern[count];
    std::cout << " pattern took " << srchCountAvg << " trials\n";

    if (verbose) {
        std::cout << "  (" << matches;
        std::cout << " instances of pattern were observed in data)\n";
    }
}
/***********************************************************************************
 * Method: GetHeads
 * Description: This method is used to retrieve the number of heads results observed
 *       since the trial beginning.
 * Usage: num_heads = cointosser_obj.GetHeads()
 * Arguments: none
 * Return: num_heads -- number of since trial start
 ***********************************************************************************/
long int Cointosser::GetHeads() {
    return number_heads;
}
/***********************************************************************************
 * Method: GetTails
 * Description: This method is used to retrieve the number of tails results observed
 *       since the trial beginning.
 * Usage: num_tails = cointosser_obj.GetTails()
 * Arguments: none
 * Return: num_tails -- number of since trial start
 ***********************************************************************************/
long int Cointosser::GetTails() {
    return number_tails;
}
/***********************************************************************************
 * Method: GetMatches
 * Description: This method is used to retrieve the number of pattern matches
 *       observed since the trial beginning.
 * Usage: num_matches = cointosser_obj.GetMatches()
 * Arguments: none
 * Return: num_matches -- number of since trial start
 ***********************************************************************************/
long int Cointosser::GetMatches() {
    return match_count_sum;
}
/***********************************************************************************
 * Constructor
 ***********************************************************************************/
Cointosser::Cointosser() {
    match_counter = 0;
    match_count_sum = 0;
    matches = 0;
    number_heads = 0;
    number_tails = 0;
}


void syntax_error() {
    std::cerr << "cointoss: incorrect syntax\n";
    std::cerr << "usage: " << COINTOSS_SYNTAX << "\n";
    std::cerr << "type 'cointoss --help' for more information\n";
}


void invalid_option( char *option) {
    std::cerr << "cointoss: invalid option '" << option << "'\n";
    std::cerr << "type 'cointoss --help' for more information\n";
}


/***********************************************************************************
 * Procedure: main
 * Description: main program execution function
 * Arguments: argc -- number of program arguments (including program name)
 *            argv -- character pointer to argument list
 ***********************************************************************************/
int main( int argc, char *argv[]) {
    int coin_toss_total = 1000000;  // total number of coin tosses (default)

    Cointosser CoinTester1;
    Cointosser CoinTester2;

    bool pattern_one[CoinTester1.PATTERNLENGTH] = {false, true, true};
    bool pattern_two[CoinTester2.PATTERNLENGTH] = {true, false, true};

    if (verbose)
        std::cout << "  cointoss program\n";

    /* read options */
    while ((argc > 1) && (argv[1][0] == '-')) {
        switch ( argv[1][1]) {
            case 'n':
                /* fall through */
            case 'N':
                /* number of iterations */
                sscanf( argv[2],"%d",&coin_toss_total);
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
                if (coin_toss_total > 100)
                    coin_toss_total = 100;
                break;

            case '-':
                {
                    char *optname_ptr = &argv[1][2];

                    if (strcmp(optname_ptr, "help") == 0) {
                        /* help */
                        std::cout << "  usage: " << COINTOSS_SYNTAX << "\n";
                        std::cout << "  options\n";
                        std::cout << "      -n  NUM         set number of cycles\n";
                        std::cout << "      -v, -verbose    enable verbose mode\n";
                        std::cout << "      -D              enable debug mode\n";
                        std::cout << "      --help          display help\n";
                        std::cout << "      --version       display version information\n";
                        return EXIT_SUCCESS;  // exit program
                    }
                    else if (strcmp(optname_ptr, "version") == 0) {
                        /* version */
                        std::cout << "  cointoss (version " << PROGAM_VERSION << ")\n";
                        return EXIT_SUCCESS;
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
                        invalid_option( bad_option);
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
                invalid_option( bad_option);
                return EXIT_FAILURE;
        }

        --argc;
        ++argv;
    }

    /* set match pattern */
    CoinTester1.SetPattern( pattern_one);
    CoinTester2.SetPattern( pattern_two);

    /* perform test */
    std::cout << "  performing analysis for " << coin_toss_total << " cycles...\n";
    cout.flush();

    for (int count = 1; count <= coin_toss_total; ++count) {
        if (debug) {
            std::cout << "  ---------- cycle number " << count << " ----------\n";
        }

        if (count < CoinTester1.PATTERNLENGTH) {
            /* preliminary coin tosses */
            CoinTester1.Toss();
            CoinTester2.Toss();
        }
        else {
            CoinTester1.TossAndCheck();
            CoinTester2.TossAndCheck();
        }
    }

    /* read and analyze data */
    if (verbose) {
        std::cout << "  sanity check: head count total for coin #1 is " << CoinTester1.GetHeads();
        std::cout << " (" << static_cast<double>(CoinTester1.GetHeads())/static_cast<double>(coin_toss_total)*100.0 << "%)\n";
        std::cout << "  sanity check: head count total for coin #2 is " << CoinTester2.GetHeads();
        std::cout << " (" << static_cast<double>(CoinTester2.GetHeads())/static_cast<double>(coin_toss_total)*100.0 << "%)\n";
    }
    CoinTester1.GenerateResults();
    CoinTester2.GenerateResults();

    return EXIT_SUCCESS;
} /* main */
