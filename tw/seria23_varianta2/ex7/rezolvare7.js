window.addEventListener("load", function() {
    window.addEventListener("keypress", function() {
        const allElements = document.querySelectorAll("*");
        for (let element of allElements) {
            if (!element.id && element.classList.length == 2) {
                element.className = element.className.split(" ").join("");
            }
        }
    });
});
