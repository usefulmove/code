import React from "react";
import { Typography } from "@mui/material";
import "./Box.css";

function Box({ name }) {
  return (
    <div className="box">
      <Typography variant="h4">Box {name}</Typography>
    </div>
  );
}

export default Box;
