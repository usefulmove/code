package com.android.coins;

import java.util.Random;

public class Coin {
    public final static boolean HEADS = true;
    public final static boolean TAILS = false;
    boolean side;
    Random rnd = new Random();

    public Coin() {
        side = HEADS;
        // note: the system clock is used by default for seeding the random number
        // generator so no explicit seeding should be necessary here in the constructor.
    }

    public void Flip() {
        if ((rnd.nextInt() % 2) == 0)
            side = HEADS;
        else
            side = TAILS;
    }

    public boolean GetSide() {
        return side;
    }

    public void SetSide( boolean value) {
        side = value;
    }
}
