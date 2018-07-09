
EASE.CelShader = {

	uniforms: {

		"celColor": { value: new THREE.Vector3(0.67, 1.0, 0.1) }

	},

	vertexShader: [

		"precision highp float;",
		"varying vec2 vUv;",
		"varying vec3 vLightFront;",

		"#include <common>",
		"#include <bsdfs>",
		"#include <lights_pars>",
		"void main() {",
			"#include <beginnormal_vertex>",
			"#include <defaultnormal_vertex>",
			"#include <begin_vertex>",
			"#include <project_vertex>",
			"#include <lights_lambert_vertex>",
			"vUv = uv;",
			"gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );",
		"}"

	].join( "\n" ),

	// http://blog.csdn.net/u011712406/article/details/50085281
	fragmentShader: [

		"varying vec3 vLightFront;",
		"uniform vec3 celColor;",

		"void main() {",
			"vec3 outColor;",
			"float intensity = vLightFront[ 0 ];",
			"float contrast = 0.4;",
			"float brightness = 1.0;",

			"if(intensity < 0.30) {",
				"outColor = mix( celColor, vec3( 0.0 ), 0.5 );",
			"} else if(intensity >= 0.95) {",
				"outColor = mix( celColor, vec3( 1.0 ), 0.3 );",
			"} else if(intensity >= 0.65) {",
				"outColor = mix( celColor, vec3( 0.5 ), 0.1 );",
			"} else if(intensity >= 0.30) {",
				"outColor = mix( celColor, vec3( 0.0 ), 0.3 );",
			"}",
// 			"outColor = ((outColor.rgb - 0.5f) * max(contrast, 0)) + 0.5f;",
			"gl_FragColor = vec4(outColor * brightness, 1.0);",
		"}"

	].join( "\n" )

};
