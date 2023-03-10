import "./App.css";
import { useState } from "react";
import { ToggleButton } from "@mui/material";
import { CropFree, HighlightOff } from "@mui/icons-material";

function App() {
  const [alignGuide, setAlignGuide] = useState(false);

  return (
    <>
      <div>
        <img src="src/assets/cam.png" width="100%" />
      </div>
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
