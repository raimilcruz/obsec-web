import React from 'react';

const style={
    color: 'rgba(0, 0, 0, 0.498039)',
    lineHeight: '22px',
    fontSize: '12px',
    marginTop: '14px',
    marginBottom: '0px'
}
export default class InputHeader extends React.Component{
    render(){
        
        return (
            <div style={style}>
                {this.props.children}
            </div>
        )
    }
}

