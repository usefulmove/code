import { useState, useEffect } from "react";
import "./App.css";
import { Container, Divider, Grid } from "@mui/material";

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
      <h1>Braking Distance Calculator</h1>
      <br />
      <Grid container spacing={2} alignItems="center">
        <Grid item className="App" xs={6}>
          <label htmlFor="weight">Weight: </label>
          <input
            id="weight"
            type="text"
            placeholder="airplane weight [lbs]"
            size="24"
            text-align="right"
          ></input>
          <br />
          <label htmlFor="pressure">Pressure: </label>
          <input
            id="pressure"
            type="text"
            placeholder="barometric pressure [inHg]"
            size="24"
          ></input>
          <br />
          <label htmlFor="temperature">Temperature: </label>
          <input
            id="temp"
            type="text"
            placeholder="ambient temperature [°C]"
            size="24"
          ></input>
          <br />
          <label htmlFor="altitude">Altitude: </label>
          <input
            id="altitude"
            type="text"
            placeholder="field elevation [ft]"
            size="24"
          ></input>
        </Grid>
        <Grid item className="App">
          <button onClick={updateBrakingDistance}>Calculate</button>
        </Grid>
      </Grid>
      <br />
      <Divider />
      <div>
        <h3>
          braking distance ={" "}
          <span className="output">{brakingDistance} ft</span> ({" "}
          {brakingDistanceMeters} m)
        </h3>
      </div>
    </Container>
  );
}

export default App;
