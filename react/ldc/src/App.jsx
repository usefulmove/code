import { useState, useEffect } from "react";
import "./App.css";
import { Container, Grid, Typography } from "@mui/material";

function App() {
  const [brakingDistance, setBrakingDistance] = useState(32.6);
  const [brakingDistanceMeters, setBrakingDistanceMeters] = useState(0);

  function updateBrakingDistance() {
    setBrakingDistance((brakingDistance) => brakingDistance + 1);
  }

  useEffect(() => {
    const distance = brakingDistance * 0.3048;
    setBrakingDistanceMeters(distance.toFixed(1));
  }, [brakingDistance]);

  return (
    <Container>
      <Typography variant="h4">Braking Distance Calculator</Typography>
      <br />
      <Grid container spacing={2} alignItems="center" direction="column">
        <Grid item className="App" xs={12}>
          <input
            id="weight"
            type="text"
            placeholder="airplane weight [lbs]"
            size="36"
            style={{ textAlign: "center" }}
          ></input>
        </Grid>
        <Grid item className="App" xs={12}>
          <input
            id="pressure"
            type="text"
            placeholder="barometric pressure [inHg]"
            size="36"
            style={{ textAlign: "center" }}
          ></input>
        </Grid>
        <Grid item className="App" xs={12}>
          <input
            id="temp"
            type="text"
            placeholder="ambient temperature [°C]"
            size="36"
            style={{ textAlign: "center" }}
          ></input>
        </Grid>
        <Grid item className="App" xs={12}>
          <input
            id="altitude"
            type="text"
            placeholder="field elevation [ft]"
            size="36"
            style={{ textAlign: "center" }}
          ></input>
        </Grid>
        <Grid item className="App" xs={12}>
          <input
            id="flaps"
            type="text"
            placeholder="flap deployment angle [°]"
            size="36"
            style={{ textAlign: "center" }}
          ></input>
        </Grid>
        <Grid item className="App">
          <span />
        </Grid>
        <Grid item className="App">
          <button onClick={updateBrakingDistance}>Calculate BD</button>
        </Grid>
      </Grid>
      <br />
      <div>
        <Typography variant="h5">
          Braking distance ={" "}
          <span className="output">{brakingDistance} feet</span> ({" "}
          {brakingDistanceMeters} meters )
        </Typography>
      </div>
    </Container>
  );
}

export default App;
