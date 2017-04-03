import React from 'react';
import RefreshIndicator from 'material-ui/RefreshIndicator';

const style = {
    refresh: {
        display: 'block',
        position: 'relative',
        margin: '0 0 8px 8px'
    }
}

export default class LoadableLine extends React.Component{
    render(){
        let content = null;
        if(this.props.loading) {
            content = <RefreshIndicator
                size={40}
                left={0}
                top={0}
                status="loading"
                style={style.refresh}
            />;
        } else{
            content = <div>{this.props.children}</div>
        }
        return (content);
    }
}