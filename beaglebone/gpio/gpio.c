#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

int debug = 0;  /* debug flag - set for additional information to console */

const int LED_OFF = 0;
const int LED_ON = 1;

void pause( double duration)
{
    clock_t clock_ticks;

    clock_ticks = clock();  /* record start time in clock ticks */
    long stop_ticks = (long)clock_ticks + (duration * CLOCKS_PER_SEC);

    while ( (long)clock() < stop_ticks)
    {
        /* do nothing */
    }
}

void SetUserLEDState( int LED_ID, int l_state)
{
    char *UserGPIOBasePath = "/sys/class/leds/beaglebone::usr";
    char filename[128];

    if (debug) printf( "SetUserLEDState( %d, %d) called\n", LED_ID, l_state);

    /* build filename */
    sprintf( filename, "/sys/class/leds/beaglebone::usr%d/brightness", LED_ID);
    if (debug) printf( "filename = %s\n", filename);

    /* open brightness file for specified LED in write mode */
    if (debug) printf( "opening %s", filename);
    FILE *file_p = fopen( filename, "w");
    if ( file_p != NULL)
    {
        /* file opened */
        if (debug) printf( "file opened successfully");

        fprintf( file_p, "%d", l_state);  /* write LED state */
        fclose( file_p);  /* close file */

        if (debug) printf( "user LED #%d set to %d\n", LED_ID, l_state);
    }
}

void FlashLED( int LED_ID, double duration)
{
    /* turn on LED */
    SetUserLEDState( LED_ID, LED_ON);

    /* wait */
    pause( duration);

    /* turn off LED */
    SetUserLEDState( LED_ID, LED_OFF);
}

int main()
{
    double blink_duration = 0.1;  /* blink duration in seconds */
    int count;
    int i = 1;
    int direction = 1;

    for ( count = 1; count <= (4 * 20); count++)
    {
        if (debug) printf( "calling FlashLED for %d and %3.2f\n", i, blink_duration);
        FlashLED( i, blink_duration);

        /* determine next LED in sequence */
        if ( i == 0) direction = 1;
        if ( i == 3) direction = -1;
        if ( direction > 0)
        {
            i++;
        }
        else
        {
            i--;
        }
    }

    return 0;
}
