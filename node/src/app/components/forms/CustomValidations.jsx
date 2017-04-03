import FormsyText from '../forms/FormsyText';

const FnRut = {
	// Valida el rut con su cadena completa "XXXXXXXX-X"
	validaRut : function (rutCompleto) {
		rutCompleto = rutCompleto.replace(new RegExp("\\.", 'g'),"");
		if (!/^[0-9]+-[0-9kK]{1}$/.test( rutCompleto ))
			return false;
		var tmp 	= rutCompleto.split('-');
		var digv	= tmp[1]; 
		var rut 	= tmp[0];
		if ( digv == 'K' ) digv = 'k' ;
		return (this.dv(rut) == digv );
	},
	dv : function(T){
		var M=0,S=1;
		for(;T;T=Math.floor(T/10))
			S=(S+T%10*(9-M++%6))%11;
		return S?S-1:'k';
	}
}

class CustomValidations{
	static validators = {
	  rut: {
	    validator: function(values, value){
	    	if(value)
	    		return FnRut.validaRut(value);
	    	else 
	    		return true;
	    },
	    message: 'Ingrese un rut válido'
	  },
	  required: {
	    validator: function(values, value) {
		  return typeof value !== 'undefined' && value !== '' && value !== false;
		},
		name: 'isRequired',
	    message: 'Este campo es requerido'
	  },
	  maxLength: (l) => {
	  	return {
		    name: "maxLength:"+l,
		    message: 'Largo máximo es '+l
	    }
	  },
	  email: {
	  	message: 'Formato de email inválido'
	  }
	}
	static map(options, name){
		const option = options.actions.POST[name];
		let vs = {'name': {}, 'message': {}};
		if(option.required){
			vs['name']['isRequired'] = true;
			vs['message']['isRequired'] = this.validators.required.message;
		}
		if(options.max_length){
			vs['name']['maxLength'] = options.max_length;
			vs['message']['maxLength'] = this.validators.maxLength(l).message;
		}
		if(option.type=="email"){
			vs['name']['isEmail'] = true;
			vs['message']['isEmail'] = this.validators.email.message;
		}
		return vs;
	}

	static mapV(options, name) {
		return this.map(options, name).name;
	}
	static mapM(options, name) {
		return this.map(options, name).message;
	}
}

Formsy.addValidationRule('isRut', CustomValidations.validators.rut.validator);
Formsy.addValidationRule('isRequired', CustomValidations.validators.required.validator);

export default CustomValidations;