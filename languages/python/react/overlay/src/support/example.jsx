import { makeStyles } from '@material-ui/core/styles';
import { Box, Icon } from '@material-ui/core';
import { CropFree } from '@material-ui/icons';

const useStyles = makeStyles((theme) => ({
  root: {
    position: 'relative',
    display: 'inline-block',
  },
  image: {
    width: '100%',
    height: 'auto',
    display: 'block',
  },
  overlay: {
    position: 'absolute',
    top: 0,
    left: 0,
    width: '100%',
    height: '100%',
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
  },
  icon: {
    color: '#fff',
    fontSize: '4rem',
  },
}));

function ImageWithOverlay() {
  const classes = useStyles();

  return (
    <div className={classes.root}>
      <img
        src="path/to/image.jpg"
        alt="Image"
        className={classes.image}
      />
      <Box className={classes.overlay}>
        <Icon component={CropFree} className={classes.icon} />
      </Box>
    </div>
  );
}

