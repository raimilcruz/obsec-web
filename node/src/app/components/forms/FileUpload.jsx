import React from 'react';

import Dropzone from 'react-dropzone';
import LinearProgress from 'material-ui/LinearProgress';
import InputHeader from './InputHeader';

const styles = {
	dropzone: {
		width:'256px', 
		height:'57px', 
		border: '1px dashed #ddd', 
		marginRight: '16px'
	},
	progress: {
		width: '187px'
	}
}
export default class FileUpload extends React.Component{
	state = {
		files: []
	}

    onDrop = (files) => {
      console.log('Received files: ', files);
      if(!this.props.multiple){
      	this.setState({files: files.slice(0,1)})
      	this.props.onFileDropped(files[0])
      } else
      	this.setState({files: files})
    }

	render(){
		const files = this.state.files.map((f,i) => {
			return (
				<div key={"file"+i}>
					<div className="fileUpload-preview">
						<img src={f.preview}  />
					</div>
					<div className="fileUpload-info">
	            		<div className="fileUpload-name">{f.name}</div>
	            		<LinearProgress mode="determinate" value={this.props.completed} style={styles.progress}/>
	            	</div>
	            </div>
				);
		});
		return (
			<div style={{clear: 'both', overflow: 'hidden'}}>
				{this.props.floatingLabelText?<InputHeader>{this.props.floatingLabelText}</InputHeader>:null}
				<Dropzone onDrop={this.onDrop} style={styles.dropzone}>
					<div style={{padding:'4px'}}>
		              	{files.length<=0?
		              		<div style={{cursor:'pointer', color:'rgba(0, 0, 0, 0.298039)'}}>Arrastre algún archivo aquí, or haga click para seleccionar el archivo a subir.</div>:
		              		<div>{files}</div>
		              	}
	              	</div>
	            </Dropzone>
            </div>
		);
	}
}