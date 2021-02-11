$(document).ready(function() {
    $("#pw-field a").on('click', function(event) {
        event.preventDefault();
        if($('#pw-field input').attr("type") == "text"){
            $('#pw-field input').attr('type', 'password');
            $('#pw-field svg').addClass( "fa-eye-slash" );
            $('#pw-field svg').removeClass( "fa-eye" );
        }else if($('#pw-field input').attr("type") == "password"){
            $('#pw-field input').attr('type', 'text');
            $('#pw-field svg').removeClass( "fa-eye-slash" );
            $('#pw-field svg').addClass( "fa-eye" );
        }
    });
});