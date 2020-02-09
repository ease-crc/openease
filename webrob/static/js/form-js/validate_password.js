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


    var lowerCaseLetters = /[a-z]/g;
    if (password.value.match(lowerCaseLetters)) {
        letter.classList.remove("invalid");
        letter.classList.add("valid");
    } else {
        letter.classList.remove("valid");
        letter.classList.add("invalid");
    }

    var upperCaseLetters = /[A-Z]/g;
    if (password.value.match(upperCaseLetters)) {
        capital.classList.remove("invalid");
        capital.classList.add("valid");
    } else {
        capital.classList.remove("valid");
        capital.classList.add("invalid");
    }


    var numbers = /[0-9]/g;
    if (password.value.match(numbers)) {
        number.classList.remove("invalid");
        number.classList.add("valid");
    } else {
        number.classList.remove("valid");
        number.classList.add("invalid");
    }


    if (password.value.length >= 6) {
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



