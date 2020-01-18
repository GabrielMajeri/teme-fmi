function isLetter(char) {
    const chr = char.toLowerCase();
    return 'a' <= chr && chr <= 'z';
}

function isDigit(char) {
    return '0' <= char && char <= '9';
}

window.addEventListener("load", function() {
    document.addEventListener("keypress", function documentKeyListener(event) {
        const key = event.key;

        const currentString = document.body.textContent.trim();

        if (isLetter(key)) {
            document.body.textContent = currentString + key;
        } else if (isDigit(key)) {
            document.body.textContent = currentString.slice(1);

            document.removeEventListener("keypress", documentKeyListener);
        }
    });
});
