using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using DebutRapid.Models;

namespace DebutRapid.Controllers
{
    public class FiguriController : Controller
    {
        public FiguriController()
        {
        }

        public IActionResult Index()
        {
            return View();
        }

        public IActionResult Prima()
        {
            Figura f = new Figura();
            f.Nume = "cerc";
            return View(f);
        }
    }
}
