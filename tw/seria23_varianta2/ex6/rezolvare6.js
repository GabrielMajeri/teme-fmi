window.addEventListener("load", function() {
    const list = document.getElementById("lista");

    const handle = setInterval(function() {
        const first = list.children[0];
        first.remove();

        list.appendChild(first);
    }, 2000);

    setTimeout(function() {
        clearInterval(handle);
    }, 20000);
});
