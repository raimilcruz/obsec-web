import React from 'react';
import RaisedButton from 'material-ui/RaisedButton';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import { Grid, Row, Col } from 'react-flexbox-grid/lib/index';

import Paper from 'material-ui/Paper';
import List from 'material-ui/List/List';
import ListItem from 'material-ui/List/ListItem';
import FontIcon from 'material-ui/FontIcon';
import Avatar from 'material-ui/Avatar';
import Divider from 'material-ui/Divider';
import Subheader from 'material-ui/Subheader';

class Test extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.handleRequestClose = this.handleRequestClose.bind(this);
    this.handleTouchTap = this.handleTouchTap.bind(this);

    this.state = {
      open: false
    };

  }

  handleRequestClose() {
    this.setState({
      open: false,
    });
  }

  handleTouchTap() {
    this.setState({
      open: true,
    });
  }


	render(){

    const standardActions = (
      <FlatButton
        label="Ok"
        secondary={true}
        onTouchTap={this.handleRequestClose}
      />
    );


		return	(
      <div>
              <Dialog
                open={this.state.open}
                title="Super Secret Password"
                actions={standardActions}
                onRequestClose={this.handleRequestClose}
              >
                1-2-3-4-5
              </Dialog>
              <h1>material-ui</h1>
              <h2>example project</h2>
              <div>
                <div>
                  <div>
                    <Paper>
                      <RaisedButton label="Secondary" secondary={true}/>
                      <List>
                        <Subheader>Tareas atrazadas</Subheader>
                        <ListItem
                          leftAvatar={<Avatar src="images/uxceo-128.jpg" />}
                          primaryText="Vanessa Peña"
                          secondaryText={
                            <p>
                              <span>10/04/2016:</span> --
                              entregar EPD
                            </p>
                          }
                          secondaryTextLines={2}
                        />
                        <Divider />
                        <ListItem
                          leftAvatar={<Avatar src="images/ok-128.jpg" />}
                          primaryText="José Miguel Herrera"
                          secondaryText={
                            <p>
                              <span>10/04/2016:</span> --
                              entregar EPD
                            </p>
                          }
                          secondaryTextLines={1}
                        />
                      <Divider />
                        <ListItem
                          leftAvatar={<Avatar src="images/kerem-128.jpg" />}
                          primaryText="Jorge Jara"
                          secondaryText={
                            <p>
                              <span>14/09/2015:</span> --
                              entregar tesis
                            </p>
                          }
                          secondaryTextLines={1}
                        />
                      </List>
                    </Paper>
                  </div>
                </div>
              </div>
              <RaisedButton
                label="Super Secret Password"
                primary={true}
                onTouchTap={this.handleTouchTap}
              />
        </div>);
     }
};


export default Test;