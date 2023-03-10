import "./App.css";
import { styled, css } from "@mui/material/styles";
import { useState } from "react";
import { Box, Icon, ToggleButton } from "@mui/material";
import { CropFree, CropFreeSharp, HighlightOff } from "@mui/icons-material";

const Overlay = styled(Box)(({ theme }) => ({
  position: "absolute",
  top: 0,
  left: 0,
  width: "100%",
  height: "100%",
  backgroundColor: "rgba(0, 0, 0, 0.25)",
  display: "flex",
  justifyContent: "center",
  alignItems: "center",
}));

const IconWrapper = styled(Icon)(() => ({
  color: "#C0B2EE",
  fontSize: "400px",
}));

function App() {
  const [alignGuide, setAlignGuide] = useState(false);

  return (
    <>
      <div style={{ position: "relative", display: "inline-block" }}>
        <img
          src="src/assets/eye.jpg"
          width="1000"
          style={{ display: "block" }}
        />
        {alignGuide ? (
          <Overlay>
            <IconWrapper component={CropFreeSharp} />
          </Overlay>
        ) : null}
      </div>
      <br />
      <ToggleButton
        value="alignmentGuide"
        selected={alignGuide}
        onChange={() => {
          setAlignGuide(!alignGuide);
        }}
        style={{
          color: "#ffffff",
          width: "75px",
          border: "1px solid #000000",
        }}
      >
        {alignGuide ? (
          <HighlightOff fontSize="large" />
        ) : (
          <CropFree fontSize="large" />
        )}
      </ToggleButton>
    </>
  );
}

export default App;
