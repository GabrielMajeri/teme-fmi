window.addEventListener("load", function() {
    const textInput = document.getElementById("txt");

    textInput.value = new Date();

    const radios = document.querySelectorAll("input[type='radio']");
    for (let radio of radios) {
        radio.name = "textStyle";
        radio.addEventListener("change", function() {
            const value = radio.nextSibling.textContent.trim();
            if (value === "bold") {
                textInput.style.fontWeight = "bold";
            } else if (value === "italic") {
                textInput.style.fontStyle = "italic";
            }

            let label = radio.parentElement;
            let para = label.parentElement;
            para.remove();
        });
    }
});
