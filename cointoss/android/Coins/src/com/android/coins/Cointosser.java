package com.android.coins;

public class Cointosser {
    Coin coin;
    CointossContainer results;
    CointossContainer match_pattern;
    int match_counter;
    int match_count_sum;
    int matches;
    int number_heads;
    int number_tails;

    public Cointosser() {
        RunOnInitialize();
    }

    public Cointosser(boolean coinface1, boolean coinface2, boolean coinface3) {
        RunOnInitialize();
        SetPattern(coinface1, coinface2, coinface3);
    }

    private void RunOnInitialize() {
        match_counter = 0;
        match_count_sum = 0;
        matches = 0;
        number_heads = 0;
        number_tails = 0;
        coin = new Coin();
        match_pattern = new CointossContainer();
        results = new CointossContainer();
    }

    public void Toss() {
        coin.Flip();

        results.add(coin.GetSide());

        if (coin.GetSide() == Coin.HEADS)
            ++number_heads;
        else
            ++number_tails;

        ++match_counter;
    }

    public void TossAndCheck() {
        Toss();

        if (match_counter < 3)
            return; /* These matches have to be rejected because the test is for
                     * number of coin tosses required to match the pattern. If
                     * these are not rejected, the program will falsely identify
                     * matches based on coin tosses from previous trials. */

        /* check for pattern match */
        if (!results.equals(match_pattern))
            return; // no match

        ++matches;
        match_count_sum += match_counter;
        match_counter = 0; // reset match counter
    }

    public void SetPattern(boolean element1, boolean element2, boolean element3) {
        match_pattern.add(element1);
        match_pattern.add(element2);
        match_pattern.add(element3);
    }

    public double GenerateResults() {
        double searchCountAverage;

        if (matches > 0)
            searchCountAverage = (double) (match_count_sum) / (double) matches;
        else
            searchCountAverage = 0;

        return searchCountAverage;
    }

    public int GetHeads() {
        return number_heads;
    }

    public int GetTails() {
        return number_tails;
    }

    public int GetMatches() {
        return match_count_sum;
    }
}
