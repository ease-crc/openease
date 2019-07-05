var myInput = document.getElementById("psw");
var letter = document.getElementById("letter");
var capital = document.getElementById("capital");
var number = document.getElementById("number");
var length = document.getElementById("length");
var check = document.getElementById("retype_password");
var submitButton = document.getElementById("register-button");


// When the user clicks on the password field, show the message box
myInput.onfocus = function () {
    document.getElementById("message").style.display = "block";
    document.getElementById("psw-check").style.display = "none";
}

// When the user clicks outside of the password field, hide the message box
myInput.onblur = function () {
    document.getElementById("message").style.display = "none";

}

// When the user starts to type something inside the password field
myInput.onkeyup = function () {
    // Validate lowercase letters

    var lowerCaseLetters = /[a-z]/g;
    if (myInput.value.match(lowerCaseLetters)) {
        letter.classList.remove("invalid");
        letter.classList.add("valid");
    } else {
        letter.classList.remove("valid");
        letter.classList.add("invalid");
    }

    // Validate capital letters
    var upperCaseLetters = /[A-Z]/g;
    if (myInput.value.match(upperCaseLetters)) {
        capital.classList.remove("invalid");
        capital.classList.add("valid");
    } else {
        capital.classList.remove("valid");
        capital.classList.add("invalid");
    }

    // Validate numbers
    var numbers = /[0-9]/g;
    if (myInput.value.match(numbers)) {
        number.classList.remove("invalid");
        number.classList.add("valid");
    } else {
        number.classList.remove("valid");
        number.classList.add("invalid");
    }

    // Validate length
    if (myInput.value.length >= 6) {
        length.classList.remove("invalid");
        length.classList.add("valid");
    } else {
        length.classList.remove("valid");
        length.classList.add("invalid");
    }

    var pword = document.getElementById("psw").value;
    var cpword = document.getElementById("retype_password").value;
    if ((pword == cpword) && (pword != "") && (cpword != "")) {
        document.getElementById('invalid-msg').style.color = 'green';
        document.getElementById('invalid-msg').innerHTML = 'matching';
    } else {
        document.getElementById('invalid-msg').style.color = 'red';
        document.getElementById('invalid-msg').innerHTML = 'not matching';
    }

}

// check if password and confirm password fields match


check.onkeyup = function () {
    var pword = document.getElementById("psw").value;
    var cpword = document.getElementById("retype_password").value;
    if ((pword == cpword) && (pword != "") && (cpword != "")) {
        document.getElementById('invalid-msg').style.color = 'green';
        document.getElementById('invalid-msg').innerHTML = 'matching';
    } else {
        document.getElementById('invalid-msg').style.color = 'red';
        // retype_password.classList.add("confirmpsw");
        document.getElementById('invalid-msg').innerHTML = 'not matching';
    }
}

submitButton.onclick = function () {

    var pword = document.getElementById("psw").value;
    var cpword = document.getElementById("retype_password").value;
    if ((pword == cpword) && (pword != "") && (cpword != "")) {

        document.getElementById('invalid-msg').style.color = 'green';
        document.getElementById('invalid-msg').innerHTML = 'matching';

    } else {

        document.getElementById('invalid-msg').style.color = 'red';
        document.getElementById('invalid-msg').innerHTML = 'not matching';

    }
}