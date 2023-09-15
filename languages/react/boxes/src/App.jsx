import React from "react";
import "./App.css";
import Box from "./components/Box";
import Column from "./components/Column";
import { Grid } from "@mui/material";

function App() {
  return (
    <div className="App">
      <Grid container justifyContent="space-between" alignItems="stretch">
        <Grid item xs={6}>
          <Column />
        </Grid>
        <Grid
          container
          direction="column"
          justifyContent="space-between"
          alignItems="flex-end"
          xs
        >
          <Grid item>
            <Box name="I" />
          </Grid>
          <Grid item>
            <Box name="II" />
          </Grid>
        </Grid>
      </Grid>
    </div>
  );
}

export default App;
