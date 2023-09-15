import React, { useState } from "react";
import pitch from "../assets/pitch2.svg";
import reference from "../assets/reference.svg";
import profile from "../assets/profile3.svg";
import { Typography } from "@mui/material";

function Gauge() {
  const [angle, setAngle] = useState(0);

  const [rotationPointX, rotationPointY] = [120, 94];

  const getRotationTransform = (angle, rotationPointX, rotationPointY) => {
    return `rotate(${angle}, ${rotationPointX}, ${rotationPointY})`;
  };

  const rotationTransform = getRotationTransform(
    angle,
    rotationPointX,
    rotationPointY
  );

  return (
    <>
      <div position="relative">
        <svg
          width="350"
          height="350"
          style={{
            position: "relative",
            left: 150,
          }}
        >
          <image href={profile} width="100%" height="100%" />
        </svg>
        <svg
          width="100"
          height="200"
          style={{
            position: "relative",
            top: -115,
            left: -200,
          }}
        >
          <image href={reference} />
        </svg>
        <svg
          width="100"
          height="200"
          style={{
            position: "relative",
            top: -115,
            left: -300,
          }}
        >
          <image href={pitch} transform={rotationTransform} />
        </svg>
        <Typography variant="h6" sx={{ color: "#a88fec" }}>
          Calculated Pitch: {Math.round(angle)}°
        </Typography>
      </div>
      <br />
      <br />
      <input
        type="number"
        value={angle}
        onChange={(event) => setAngle(event.target.value)}
        placeholder="angle"
      />
    </>
  );
}

export default Gauge;
