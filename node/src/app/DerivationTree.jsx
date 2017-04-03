import React from 'react';
import $ from 'jquery';
import Paper from 'material-ui/Paper';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import request from 'superagent';
import Subheader from 'material-ui/Subheader';
import LoadableContent from './components/LoadableContent';
const update = require('react-addons-update');
import MoreVert from 'material-ui/svg-icons/navigation/more-vert';
import ArrowDropDown from 'material-ui/svg-icons/navigation/arrow-drop-down';
import ArrowDropUp from 'material-ui/svg-icons/navigation/arrow-drop-up';
import SvgIcon from 'material-ui/SvgIcon';
var ReactDOM = require('react-dom');



const ExpandIcon = (props) => (
	<SvgIcon {...props}>
		<path d="M9.5,13.09L10.91,14.5L6.41,19H10V21H3V14H5V17.59L9.5,13.09M10.91,9.5L9.5,10.91L5,6.41V10H3V3H10V5H6.41L10.91,9.5M14.5,13.09L19,17.59V14H21V21H14V19H17.59L13.09,14.5L14.5,13.09M13.09,9.5L17.59,5H14V3H21V10H19V6.41L14.5,10.91L13.09,9.5Z" />
	</SvgIcon>
);

const CompressIcon = (props) => (
	<SvgIcon {...props}>
		<path d="M19.5,3.09L20.91,4.5L16.41,9H20V11H13V4H15V7.59L19.5,3.09M20.91,19.5L19.5,20.91L15,16.41V20H13V13H20V15H16.41L20.91,19.5M4.5,3.09L9,7.59V4H11V11H4V9H7.59L3.09,4.5L4.5,3.09M3.09,19.5L7.59,15H4V13H11V20H9V16.41L4.5,20.91L3.09,19.5Z"  />
	</SvgIcon>
);

export default class DerivationTree extends React.Component{
	state = {
		open: {},
		allOpen: false,
		showOptions: false,
		loading: false,
		loadingPremises: true,
		premiseRendered: false
	}

	shouldComponentUpdate(nextProps, nextState){
		return nextProps.shouldUpdate!=undefined?nextProps.shouldUpdate:true;
	}

	componentDidMount() {
		/*let dom = $(ReactDOM.findDOMNode(this.refs.tree)).find(".dtree>.infer>.conclusion").get(0);
		MathJax.Hub.Queue(["Typeset",MathJax.Hub, dom], () => {
			let envdom = $(ReactDOM.findDOMNode(this.refs.tree)).find(".envstore").get(0);
			MathJax.Hub.Queue(["Typeset",MathJax.Hub, envdom], () => this.setState({loading: false}));
		});*/

	}

	buildDerivationTree = (tree, key) => {
		const premises = tree.subtrees.map((p, i) => this.buildDerivationTree(p, key+':'+i));
		const judgments = tree.judgments.map((p, i) => <div className="judgment" key={"judgment+"+i}>{"$$"+p.judgment+"$$"}</div>);
		return (<div className="infer" key={"infer"+key}>
			{key <=0 && !this.state.premiseRendered?null:
			<div className="premise" style={{display: this.state.allOpen || premises.length<=0 || this.state.open[key]?'':'none'}}>
				{key<=0?
					<div>
						<LoadableContent loading={this.state.loadingPremises}></LoadableContent>
						<div style={{display: this.state.loadingPremises?"none":""}}>
							{premises} {judgments}
						</div>
					</div>:
					<div>
						{premises} {judgments}
					</div>
				}

			</div>}
			<div className="premise premise-more" style={{display: this.state.allOpen || (premises.length<=0 && judgments.length<=0) || this.state.open[key]?'none':''}} onClick={this.expand(key)}>
				<MoreVert />
			</div>
			<div className="conclusion">{"$$"+tree.term+"$$"}</div>
		</div>);
	}

	mathjaxPremises = () => {
		if(this.state.loadingPremises){
			let dom = $(ReactDOM.findDOMNode(this.refs.tree)).find(".dtree").get(0);
			MathJax.Hub.Queue(["Typeset",MathJax.Hub, dom], () => this.setState({loadingPremises: false}));
		}
	}

	expand = (key) => () =>  {

		this.setState({premiseRendered: true, open: update(this.state.open, {[key]: {$set: true}})}, () => this.mathjaxPremises());
	}

	showOptions = () => {
		this.setState({showOptions: true});
	}
	hideOptions = () => {
		this.setState({showOptions: false});
	}
	expandAll = (b) => () => {
		if(!b){
			this.setState({premiseRendered: true, open: {}, allOpen: b}, () => this.mathjaxPremises());
		} 
		else 
			this.setState({premiseRendered: true, allOpen: b}, () => this.mathjaxPremises());
		
	}


	render(){



		const dtree = this.props.tree?this.buildDerivationTree(this.props.tree, 0):null;

		return (
			<Paper onMouseEnter={this.showOptions} onMouseLeave={this.hideOptions} style={{position: 'relative', padding:'4px', overflow: 'auto'}} ref="tree">
				<LoadableContent loading={this.state.loading}></LoadableContent>
				<div className="row bottom-xs" style={{display: this.state.loading?'none':''}}>
					<div className="col-xs-12 dtree">
						{dtree}
						<div style={{position:'absolute', top:'8px', right:'16px',width:'24px', height:'24px'}}>
							{this.state.showOptions?
							<div style={{cursor:'pointer'}}>
								{this.state.allOpen?<CompressIcon onClick={this.expandAll(false)}/>:<ExpandIcon onClick={this.expandAll(true)}/>}
							</div>:null}
						</div>
					</div>
					<div className="col-xs-12 end-xs envstore">
						<div style={{clear:'both', overflow:'hidden'}}></div>
						{this.props.env && this.props.substitutionMode==0?
						<div style={{textAlign: 'right'}}>
							<div>
								Environment: {"\\("+this.props.env+"\\)"}
							</div>
						</div>:null}

						{this.props.store?
						<div style={{textAlign: 'right'}}>
							<div>
								Store: {"\\("+this.props.store+"\\)"}
							</div>
						</div>:null}
					</div>
				</div>
			</Paper>
			);
	}
}