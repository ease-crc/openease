var password = document.getElementById("psw");
var letter = document.getElementById("letter");
var capital = document.getElementById("capital");
var number = document.getElementById("number");
var length = document.getElementById("length");
var retype_psword = document.getElementById("retype_password");


password.onfocus = function () {
    document.getElementById("message").style.display = "block";
    document.getElementById("psw-check").style.display = "none";
}


password.onblur = function () {
    document.getElementById("message").style.display = "none";
}


password.onkeyup = function () {

    if (password.value.length >= 2) {
        length.classList.remove("invalid");
        length.classList.add("valid");
    } else {
        length.classList.remove("valid");
        length.classList.add("invalid");
    }


    if ((password == retype_psword) && (password != "") && (retype_psword != "")) {
        document.getElementById('feedback-msg').innerHTML = 'Retype Password matches';
        document.getElementById('feedback-msg').style.color = 'green';
    } else {

        document.getElementById('feedback-msg').innerHTML = 'Retype Password field is Invalid. Does not match with Password';
        document.getElementById('feedback-msg').style.color = 'red';
    }

}


retype_psword.onkeyup = function () {

    if ((password == retype_psword) && (password != "") && (retype_psword != "")) {
        document.getElementById('feedback-msg').innerHTML = 'Retype Password field matches';
        document.getElementById('feedback-msg').style.color = 'green';
    } else {

        document.getElementById('feedback-msg').innerHTML = 'Retype Password field is Invalid. Does not match with Password';
        document.getElementById('feedback-msg').style.color = 'red';
    }
}



