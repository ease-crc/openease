// 'use strict';

EASE.CelShadingPass = function (scene, camera) {
	
	if ( EASE.CelShader === undefined) {
		console.warn( 'EASE.CelShadingPass depends on EASE.CelShader' );
		return new THREE.ShaderPass();
	}
	
	var cartoonMaterial = new THREE.ShaderMaterial( {
		lights: true,
		uniforms: Object.assign(
			THREE.UniformsLib[ "common" ],
			THREE.UniformsLib[ "lights" ],
			EASE.CelShader.uniforms ),
		vertexShader: EASE.CelShader.vertexShader,
		fragmentShader: EASE.CelShader.fragmentShader
	});
	
	THREE.RenderPass.call(this, scene, camera, cartoonMaterial);
	
// 	this.markers = [];
}

EASE.CelShadingPass.prototype = Object.create( THREE.RenderPass.prototype );

EASE.CelShadingPass.prototype.addMarker = function(marker,node) {
// 	var cartoonMaterial = new THREE.ShaderMaterial( {
// 		lights: true,
// 		uniforms: Object.assign(
// 			THREE.UniformsLib[ "common" ],
// 			THREE.UniformsLib[ "lights" ],
// 			EASE.CelShader.uniforms ),
// 		vertexShader: EASE.CelShader.vertexShader,
// 		fragmentShader: EASE.CelShader.fragmentShader
// 	});
// 	this.markers.push({marker: marker, node: node, material: cartoonMaterial});
};

EASE.CelShadingPass.prototype.removeMarker = function(marker,node) {
// 	var index = this.markers.indexOf({marker: marker, node: node}); // TODO not sure if this works
// 	if (index > -1) {
// 		this.markers.splice(index, 1);
// 	}
};

EASE.CelShadingPass.prototype.render = function( renderer, writeBuffer, readBuffer, delta, maskActive ) {
	
	// TODO: use only one material and use scene.overrideMaterial
// 	for(var i=0; i<this.markers.length; i++) {
// 		var x = this.markers[i];
// 		var color    = x['marker'].msgColor;
// 		var material = x['material'];
// 		var node     = x['node'];
// 		material.uniforms['celColor'].value.set(color.r,color.g,color.b);
// // 		material.uniforms['celColor'].value.set(0.2,0.7,0.4);
// 		node.traverse(function(currentObject) {
// 			if(currentObject.material) {
// 				currentObject.celMaterial = currentObject.material;
// 				currentObject.material = material;
// 			}
// 		});
// 	}
	
	THREE.RenderPass.prototype.render.call(this, renderer, writeBuffer, readBuffer, delta, maskActive);
	
// 	for(var i=0; i<this.markers.length; i++) {
// 		var x = this.markers[i];
// 		x['node'].traverse(function(currentObject) {
// 			if(currentObject.celMaterial) {
// 				currentObject.material = currentObject.celMaterial;
// 			}
// 		});
// 	}
	
// 	this.scene.traverse(unsetComicColor);
};
