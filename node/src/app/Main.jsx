/**
 * In this file, we create a React component
 * which incorporates components providedby material-ui.
 */

import React from 'react';
import styleResizable from 'material-ui/utils/styleResizable';
import spacing from 'material-ui/styles/spacing';



import {deepOrange500, blueGrey500} from 'material-ui/styles/colors';

import getMuiTheme from 'material-ui/styles/getMuiTheme';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import AppBar from 'material-ui/AppBar';





import darkBaseTheme from 'material-ui/styles/baseThemes/darkBaseTheme';
import lightBaseTheme from 'material-ui/styles/baseThemes/lightBaseTheme';





const darkMuiTheme = getMuiTheme(darkBaseTheme);

const lightMuiTheme = getMuiTheme(lightBaseTheme);



const muiTheme = getMuiTheme({
  palette: {
    accent1Color: deepOrange500,
    primary1Color: blueGrey500,
  }
});

const thresshold = 768

class Main extends React.Component {
  static contextTypes = {
    router : React.PropTypes.object
  }

  constructor(props, context) {
    super(props, context);
    
    this.handleTouchTapLeftIconButton = this.handleTouchTapLeftIconButton.bind(this);
    this.handleChangeRequestNavDrawer = this.handleChangeRequestNavDrawer.bind(this);
    this.handleRequestChangeList = this.handleRequestChangeList.bind(this);

    this.handleResize = this.handleResize.bind(this);
    window.addEventListener('resize', this.handleResize);

    this.state = {
      navDrawerOpen: window.innerWidth>thresshold,
      navDrawerDocked: window.innerWidth>thresshold,
      isDeviceSize: window.innerWidth<=thresshold
    };

  }

  handleResize() {
    if(this.state.navDrawerOpen && window.innerWidth < thresshold)
      this.setState({navDrawerOpen: false, isDeviceSize: true, navDrawerDocked: false});
    else if(!this.state.navDrawerOpen && window.innerWidth >= thresshold)
      this.setState({navDrawerOpen: true, isDeviceSize: false, navDrawerDocked: true});
  }


  handleTouchTapLeftIconButton() {
    this.setState({
      navDrawerOpen: !this.state.navDrawerOpen,
    });
  }

  handleChangeRequestNavDrawer(open) {
    this.setState({
      navDrawerOpen: open,
    });
  }

  handleRequestChangeList(event, value) {
    this.context.router.push(value);
    this.setState({
      navDrawerOpen: false,
    });
  }


  getStyles = () => {
    const styles = {
      root: {
        minHeight: 400,
      }
    }
    return styles
  }

  isDeviceSize = (size) => {
    if(this.state.isDeviceSize && size == styleResizable.statics.Sizes.LARGE)
      return false;
    else return true;
  }

  onRequestChange = (open) => {
    this.setState({navDrawerOpen: open});
  }
  render() {
    
    let showMenuIconButton = true;
    const styles = this.getStyles();




    return (
      <div>
      <MuiThemeProvider muiTheme={muiTheme}>
        <div>
          <div className="main" style={styles.root}>
            <AppBar onLeftIconButtonTouchTap={this.handleTouchTapLeftIconButton} title="GObSec Pad"
            showMenuIconButton={false} style={styles.appBar}
            />
            <div className="content">
              {this.props.children}
            </div>
          </div>
        </div>
      </MuiThemeProvider>
      </div>
    );
  }
}

export default Main;
