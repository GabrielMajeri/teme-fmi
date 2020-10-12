using Microsoft.AspNetCore.Mvc;
using Laborator2.Models;

namespace Laborator2.Controllers
{
    // Vreau ca toate aceste acțiuni din controller să fie accesibile fără prefix,
    // direct la localhost:5000/
    [Route("/")]
    public class HomeController : Controller
    {
        public HomeController()
        {
        }

        // Verifică dacă șirul `substr` este conținut în șirul `input`
        [HttpGet("Route1/{input}/{substr}")]
        public IActionResult Route1(string input, string substr)
        {
            // Transmit view-ului datele
            return View(new Route1Model {
                Input = input,
                Substring = substr
            });
        }

        // La fel ca mai sus, dar al doilea parametru este opțional
        [HttpGet("Route2/{input}/{substr?}")]
        public IActionResult Route2(string input, string substr)
        {
            return View(new Route1Model {
                Input = input,
                Substring = substr
            });
        }

        // Rută care acceptă doar numere pare, de 3-7 cifre.
        //
        // Am construit expresia regulată cu https://regexr.com/
        //
        // Folosesc un regex cu:
        // - o cifră diferită de 0
        // - apoi 1-5 cifre
        // - și apoi o cifră pară
        //
        // Trebuie să punem {{ și }} și [[ și ]], în loc de doar {, }, [, ],
        // ca să nu le interpreteze greșit ASP.NET
        [HttpGet("Route3/{value:regex(^[[1-9]][[0-9]]{{1,5}}[[0,2,4,6,8]]$)}")]
        public IActionResult Route3(int value)
        {
            return Ok("Hello");
        }

        // Primește o listă de numere, și folosește un view parțial
        // ca să afișeze care sunt divizibile cu 7.
        //
        // Cum se apelează această rută:
        // se accesează /Route4?values=1&values=2&values=3&....
        [HttpGet("Route4")]
        public IActionResult Route4(int[] values)
        {
            return View(values);
        }

        // Acceptă doar șirurile de forma a^n b^n cu n > 0
        //
        // Rezolvare cu regex bazată pe
        // https://docs.microsoft.com/en-us/dotnet/standard/base-types/grouping-constructs-in-regular-expressions#balancing-group-definitions
        [HttpGet("Route5/{input:regex(^(?'A'a)+(?'B-A'b)+(?(A)(?!))$)}")]
        public IActionResult Route5(string input)
        {
            return Ok("Succesful match");
        }
    }
}
