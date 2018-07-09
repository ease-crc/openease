
EASE.OutlineShader = {

	uniforms: {

		"tDepth": { value: null },
		"tNormal": { value: null },
		"tDiffuse": { value: null }

	},

	vertexShader: [

		"precision highp float;",
		"varying vec2 vUv;",
		"varying vec3 vLightFront;",

		"#include <common>",
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

	fragmentShader: [

		"uniform sampler2D tDepth;",
		"uniform sampler2D tNormal;",
		"uniform sampler2D tDiffuse;",
		"varying vec2 vUv;",
		
		"float planeDistance( const in vec3 positionA, const in vec3 normalA, const in vec3 positionB, const in vec3 normalB ) {",
			"vec3 positionDelta = positionB-positionA;",
			"float planeDistanceDelta = max( abs( dot( positionDelta, normalA ) ), abs( dot( positionDelta, normalB ) ) );",
			"return planeDistanceDelta;",
		"}",

		"void main() {",
			"float depthCenter = texture2D( tDepth, vUv ).r;",
			"float px = 1.0 / 1000.0;",

			"vec3 leftpos  = vec3( vUv.s - px, vUv.t, 1.0 - texture2D( tDepth, vec2( vUv.s - px, vUv.t ) ).r );",
			"vec3 rightpos = vec3( vUv.s + px, vUv.t, 1.0 - texture2D( tDepth, vec2( vUv.s + px, vUv.t ) ).r );",
			"vec3 uppos    = vec3( vUv.s, vUv.t - px, 1.0 - texture2D( tDepth, vec2( vUv.s, vUv.t - px ) ).r );",
			"vec3 downpos  = vec3( vUv.s, vUv.t + px, 1.0 - texture2D( tDepth, vec2( vUv.s, vUv.t + px ) ).r );",

			"vec3 leftnor  = texture2D( tNormal, vec2( vUv.s - px, vUv.t ) ).xyz;",
			"vec3 rightnor = texture2D( tNormal, vec2( vUv.s + px, vUv.t ) ).xyz;",
			"vec3 upnor    = texture2D( tNormal, vec2( vUv.s, vUv.t - px ) ).xyz;",
			"vec3 downnor  = texture2D( tNormal, vec2( vUv.s, vUv.t + px ) ).xyz;",

			"vec2 planeDist = vec2( planeDistance( leftpos, leftnor, rightpos, rightnor ), ",
			"                       planeDistance( uppos, upnor, downpos, downnor ) );",
			"float planeEdge = 2.5 * length( planeDist );",
			"planeEdge = 1.0 - 0.5 * smoothstep( 0.0, depthCenter, planeEdge );",
			"float normEdge = max( length( leftnor - rightnor ), length( upnor - downnor ) );",
			"normEdge = 1.0 - 0.5 * smoothstep( 0.0, 0.2, normEdge ); ",
// 			"float edge= planeEdge * normEdge;",
			"float edge= 0.5 * (planeEdge + normEdge);",
			"vec4 raw = texture2D( tDiffuse, vUv );",
			"gl_FragColor = vec4( vec3( raw * edge ), 1.0 );",
// 			"gl_FragColor = vec4( raw.xyz, 1.0 );",
// 			"gl_FragColor = vec4( vUv, 0.0, 1.0 );",
		"}"

	].join( "\n" )

};
