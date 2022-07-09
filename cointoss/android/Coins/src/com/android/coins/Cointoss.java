package com.android.coins;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.TextView;
import android.view.View;
import android.view.Menu;
import android.view.ContextMenu;
import android.view.MenuItem;

public class Cointoss extends Activity {
    Coin[] coin;
    ImageButton[] coin_button;
    Button start_button;
    ProgressDialog progress;
    AlertDialog.Builder dialog;
    TextView hidden_area_right;
    TextView hidden_area_left;
    int numTossesTotal;
    public static final int SMALLEST_ID = Menu.FIRST + 1;
    public static final int SMALL_ID = Menu.FIRST + 2;
    public static final int MEDIUM_ID = Menu.FIRST + 3;
    public static final int LARGE_ID = Menu.FIRST + 4;
    public static final int LARGEST_ID = Menu.FIRST + 5;
    public static final String COIN_STATE_1_KEY = "COINONE";
    public static final String COIN_STATE_2_KEY = "COINTWO";
    public static final String COIN_STATE_3_KEY = "COINTHREE";


    Handler background_handler = new Handler() {
        @Override
        public void handleMessage( Message msg) {
            if (msg.arg1 == 999) {
                dialog.setTitle( "Calculation Complete");
                String dialog_message = "The average number of coin tosses required to match that pattern " +
                                         GenerateSeqString( coin[0].side, coin[1].side, coin[2].side) + " was " +
                                         String.format( "%2.3f", (float)msg.arg2/1000.0) + ".";
                dialog.setMessage( dialog_message);
                dialog.setIcon( R.drawable.icon2);
                dialog.setNeutralButton( "Ok", new DialogInterface.OnClickListener() {
                    public void onClick( DialogInterface dinterface, int value) {
                        progress.hide();  // hide progress dialog
                        EnableGUI( true);
                        // dialog will close without any explicit code here
                    }
                });
                dialog.show();  // display dialog
            }
        }
    };

    @Override
    public void onCreate( Bundle savedInstanceState) {
        super.onCreate( savedInstanceState);
        setContentView( R.layout.main);

        /* initialize variables and memory */
        coin = new Coin[3];
        coin[0] = new Coin();
        coin[1] = new Coin();
        coin[2] = new Coin();
        dialog = new AlertDialog.Builder( this);
        numTossesTotal = 100000;

        /* show application version */
        TextView version = (TextView)findViewById( R.id.version);
        String version_name;
        try {
            version_name = this.getPackageManager().getPackageInfo( "com.android.coins", 0).versionName;
        } catch (NameNotFoundException e) {
            version_name = "";
            e.printStackTrace();
        }
        version.setText( version_name);

        /* configure start button */
        start_button=(Button)findViewById( R.id.start_button);
        start_button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                EnableGUI( false);
                progress.show();  // display progress dialog

                Thread background = new Thread( new Runnable() {
                    public void run() {
                        double result = RunSimulation( coin[0].side, coin[1].side, coin[2].side);

                        /* pass message back to GUI thread */
                        Message msg = new Message();
                        msg.arg1 = 999;
                        msg.arg2 = (int)(result*1000.0);  // pass result back in munits
                        background_handler.sendMessage( msg);
                    }
                });
                background.start();
            }
        });

        /* configure coin buttons */
        coin_button = new ImageButton[3];
        coin_button[0]=(ImageButton)findViewById( R.id.coin_1_button);
        coin_button[1]=(ImageButton)findViewById( R.id.coin_2_button);
        coin_button[2]=(ImageButton)findViewById( R.id.coin_3_button);

        coin_button[0].setOnClickListener(new View.OnClickListener() {
            public void onClick( View v) {
                ToggleButtonState(0);
            }
        });
        coin_button[1].setOnClickListener(new View.OnClickListener() {
            public void onClick( View v) {
                ToggleButtonState(1);
            }
        });
        coin_button[2].setOnClickListener(new View.OnClickListener() {
            public void onClick( View v) {
                ToggleButtonState(2);
            }
        });

        /* configure progress bar */
        progress = new ProgressDialog( this);
        progress.setMessage( "running simulation...");
        progress.setTitle( "cointoss");
        //progress.setIndeterminate( true);
        progress.setCancelable( false);
        progress.setIcon( R.drawable.icon2);
        progress.setProgressStyle( ProgressDialog.STYLE_HORIZONTAL);

        /* configure hidden areas (used to call up context menu for settings) */
        hidden_area_left = (TextView)findViewById( R.id.hidden_left);
        hidden_area_left.setOnCreateContextMenuListener(new View.OnCreateContextMenuListener() {
            public void onCreateContextMenu( ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
                PopulateMenu( menu);
                menu.setHeaderTitle( "Choose No. of Tosses");
                menu.setHeaderIcon( R.drawable.icon2);
            }
        });
        hidden_area_right = (TextView)findViewById( R.id.hidden_right);
        hidden_area_right.setOnCreateContextMenuListener(new View.OnCreateContextMenuListener() {
            public void onCreateContextMenu( ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
                PopulateMenu( menu);
                menu.setHeaderTitle( "Choose No. of Tosses");
                menu.setHeaderIcon( R.drawable.icon2);
            }
        });

        for (int i = 0; i < 3; i++) {
            ResolveButtonState( i);
        }
    }

    @Override
    public void onSaveInstanceState( Bundle outState) {
        super.onSaveInstanceState( outState);
        outState.putBoolean( COIN_STATE_1_KEY, coin[0].GetSide());
        outState.putBoolean( COIN_STATE_2_KEY, coin[1].GetSide());
        outState.putBoolean( COIN_STATE_3_KEY, coin[2].GetSide());
    };

    @Override
    public void onRestoreInstanceState( Bundle savedInstanceState) {
        super.onRestoreInstanceState( savedInstanceState);
        // restore saved state
        if ( savedInstanceState != null) {
            coin[0].SetSide( savedInstanceState.getBoolean( COIN_STATE_1_KEY));
            coin[1].SetSide( savedInstanceState.getBoolean( COIN_STATE_2_KEY));
            coin[2].SetSide( savedInstanceState.getBoolean( COIN_STATE_3_KEY));
        }
        for (int i = 0; i < 3; i++) {
            ResolveButtonState( i);
        }
    }

    private void ToggleButtonState( int i) {
        coin[i].SetSide( !coin[i].GetSide());
        ResolveButtonState( i);
    }

    private void ResolveButtonState( int i) {
        if (coin[i].GetSide() == Coin.HEADS) {
            coin_button[i].setImageResource( R.drawable.heads);
        } else {
            coin_button[i].setImageResource( R.drawable.tails);
        }
    }

    private void EnableGUI( boolean state) {
        for (int i = 0; i < 3; i++) {
            coin_button[i].setEnabled( state);
            if (state)
                coin_button[i].setAlpha( 255);
            else
                coin_button[i].setAlpha(128);
        }
        start_button.setEnabled( state);
        hidden_area_left.setEnabled( state);
        hidden_area_right.setEnabled( state);
    }

    private String GenerateSeqString( boolean value1, boolean value2, boolean value3) {
        String c1, c2, c3;

        if (value1)
            c1 = "heads";
        else
            c1 = "tails";
        if (value2)
            c2 = "heads";
        else
            c2 = "tails";
        if (value3)
            c3 = "heads";
        else
            c3 = "tails";

        String seq_string = "(" + c1 + ":" + c2  + ":" + c3 + ")";
        return seq_string;
    }

    private double RunSimulation( boolean coin1side, boolean coin2side, boolean coin3side) {
        Cointosser tosser = new Cointosser();

        /* set match pattern */
        tosser.SetPattern( coin1side, coin2side, coin3side);

        progress.setMax( numTossesTotal);

        for (int count = 1; count <= numTossesTotal; count++) {
            if (count < 3)
                tosser.Toss();
            else
                tosser.TossAndCheck();

            if ( count % 1000 == 0)
                progress.setProgress( count);
        }

        return tosser.GenerateResults();
    }

    @Override
    public boolean onCreateOptionsMenu( Menu menu) {
        // PopulateMenu( menu);
        return( super.onCreateOptionsMenu( menu));
    }

    @Override
    public boolean onOptionsItemSelected( MenuItem item) {
        return( ApplyMenuChoice( item) || super.onOptionsItemSelected( item));
    }

    @Override
    public boolean onContextItemSelected( MenuItem item) {
        return( ApplyMenuChoice( item) || super.onContextItemSelected( item));
    }

    private void PopulateMenu( Menu menu) {
        menu.add(0, SMALLEST_ID, 0,     "1,000");
        menu.add(0,    SMALL_ID, 1,    "50,000");
        menu.add(0,   MEDIUM_ID, 2,   "100,000 (default)");
        menu.add(0,    LARGE_ID, 3,   "500,000");
        menu.add(0,  LARGEST_ID, 4, "1,000,000 (slow)");
    }

    private boolean ApplyMenuChoice( MenuItem item) {
        switch (item.getItemId()) {
            case SMALLEST_ID:
                numTossesTotal = 1000;
                return true;
            case SMALL_ID:
                numTossesTotal = 50000;
                return true;
            case MEDIUM_ID:
                numTossesTotal = 100000;
                return true;
            case LARGE_ID:
                numTossesTotal = 500000;
                return true;
            case LARGEST_ID:
                numTossesTotal = 1000000;
                return true;
        }
        return false;
    }
}
