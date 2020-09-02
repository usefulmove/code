package com.android.coins;

public class CointossContainer {
    int size;
    boolean tosses[];

    public CointossContainer() {
        size = 0;
        tosses = new boolean[3];
    }

    public void add( boolean element) {
        for (int i = 2; i > 0; i--) {
            tosses[i] = tosses[i - 1];
        }

        tosses[0] = element;

        if (size < 3)
            ++size;
    }

    public boolean get( int i) {
        return tosses[i];
    }

    public boolean equals( CointossContainer crc) {
        for (int i = 0; i < 3; i++) {
            if (tosses[i] != crc.get(i))
                return false;
        }
        return true;
    }

    public int GetSize() {
        return size;
    }
}
