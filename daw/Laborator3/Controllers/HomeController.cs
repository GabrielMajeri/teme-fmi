using Microsoft.AspNetCore.Mvc;

namespace Laborator3.Controllers
{
    public class HomeController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }
    }
}
