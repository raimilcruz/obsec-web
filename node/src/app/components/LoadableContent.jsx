import React from 'react';
import RefreshIndicator from 'material-ui/RefreshIndicator';

const style = {
    container: {
        position: 'relative',
        textAlign: 'center'
    },
    refresh: {
        display: 'inline-block',
        position: 'relative',
    }
}

export default class LoadableContent extends React.Component{
    render(){
        let content = null;
        if(this.props.loading) {
            content = 
            <div style={style.container}>
            <RefreshIndicator
                size={100}
                left={0}
                top={0}
                status="loading"
                style={style.refresh}
            /></div>;
        } else{
            content = <div>{this.props.children}</div>
        }
        return (content);
    }
}