import React from "react";
import "./App.css";
import Box from "./components/Box";
import Column from "./components/Column";
import { Grid } from "@mui/material";

function App() {
  return (
    <div className="App">
      <Grid container>
        <Grid item xs={8} justifyContent="flex-start">
          <Column />
        </Grid>
        <Grid item xs={4} alignItems="center">
          <div>
            <Box name="I" />
          </div>
          <div>
            <Box name="II" />
          </div>
        </Grid>
      </Grid>
    </div>
  );
}

export default App;
