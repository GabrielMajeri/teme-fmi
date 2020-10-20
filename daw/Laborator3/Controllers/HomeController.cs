using System.Linq;
using Laborator3.Models;
using Microsoft.AspNetCore.Mvc;

namespace Laborator3.Controllers
{
    public class HomeController : Controller
    {
        private AppDbContext context = new AppDbContext();

        public HomeController()
        {
            context.DropCreate();
            context.Seed();
        }

        public IActionResult Index()
        {
            return View();
        }

        [HttpGet("books")]
        public IActionResult Books()
        {
            var books = (IQueryable<Book>)context.Books;
            return View(books);
        }

        [HttpGet("book/{id}")]
        public IActionResult BookDetails(int id)
        {
            var book = context.Books.Find(id);
            if (book == null)
            {
                return NotFound();
            }
            return View(book);
        }
    }
}
