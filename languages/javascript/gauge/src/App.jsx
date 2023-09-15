import { Box, Typography } from "@mui/material";
import "./App.css";
import Gauge from "./components/Gauge";

function App() {
  return (
    <>
      <Typography variant="h5">Pitch Angle ( Proto )</Typography>
      <Box
        justifyContent="center"
        alignContent="center"
        height="100%"
        width="100%"
      >
        <Gauge />
      </Box>
    </>
  );
}

export default App;
