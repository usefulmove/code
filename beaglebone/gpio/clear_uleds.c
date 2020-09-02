#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

int debug = 0;  /* debug flag - set for additional information to console */

const int LED_OFF = 0;
const int LED_ON = 1;

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

int main()
{
    if (debug) printf( "debug mode\n");

    int i;
    for (i = 0; i <= 3; i++)
    {
        SetUserLEDState( i, LED_OFF);
    }

    return 0;
}
