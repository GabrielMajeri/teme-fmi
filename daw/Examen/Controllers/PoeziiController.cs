using System.Collections.Generic;
using System.Threading.Tasks;
using System.Linq;
using Examen.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using Microsoft.EntityFrameworkCore;
using Examen.Models;
using System.ComponentModel.DataAnnotations;

namespace Examen.Controllers
{
    public class PoeziiController : Controller
    {
        private readonly AppDbContext _context;

        public PoeziiController(AppDbContext context)
        {
            _context = context;
        }

        public async Task<IActionResult> Index()
        {
            var poezii = await _context.Poezii
                .Include(p => p.Volum)
                .AsNoTracking()
                .ToListAsync();
            return View(poezii);
        }

        [HttpGet]
        public async Task<IActionResult> Create()
        {
            var editPoezieViewModel = new EditPoezieViewModel
            {
                Volume = await GetSelectListVolume()
            };
            return View(editPoezieViewModel);
        }

        [HttpPost]
        public async Task<IActionResult> Create([Required] EditPoezieViewModel vm)
        {
            if (ModelState.IsValid)
            {
                _context.Add(vm.Poezie);
                await _context.SaveChangesAsync();
                return RedirectToAction("Index");
            }
            else
            {
                vm.Volume = await GetSelectListVolume();
                return View(vm);
            }
        }

        [HttpGet("Edit/{id}")]
        public async Task<IActionResult> Edit(int id)
        {
            var editPoezieViewModel = new EditPoezieViewModel
            {
                Poezie = await _context.Poezii.FindAsync(id),
                Volume = await GetSelectListVolume()
            };
            return View(editPoezieViewModel);
        }

        [HttpPost("Edit/{id}")]
        public async Task<IActionResult> Edit(int id, EditPoezieViewModel vm)
        {
            vm.Poezie.Id = id;
            if (ModelState.IsValid)
            {
                _context.Update(vm.Poezie);
                await _context.SaveChangesAsync();
                return RedirectToAction("Index");
            }
            else
            {
                vm.Volume = await GetSelectListVolume();
                return View(vm);
            }
        }

        [HttpGet("Delete/{id}")]
        public IActionResult Delete()
        {
            return View();
        }

        [HttpPost("Delete/{id}")]
        public async Task<IActionResult> Delete(int id)
        {
            var poezie = await _context.Poezii.FindAsync(id);
            _context.Poezii.Remove(poezie);
            await _context.SaveChangesAsync();
            return RedirectToAction("Index");
        }

        [HttpGet("FindByTitle/{substr}")]
        public async Task<IActionResult> FindByTitle(string substr)
        {
            var poezii = await _context.Poezii.Include(p => p.Volum)
                .AsNoTracking()
                .Where(p => p.Titlu.Contains(substr))
                .ToListAsync();

            return View(poezii);
        }

        [HttpGet("FindByVolume/{substr}")]
        public async Task<IActionResult> FindByVolume(string substr)
        {
            var poezii = await _context.Poezii.Include(p => p.Volum)
                .AsNoTracking()
                .Where(p => p.Volum.Denumire.Contains(substr))
                .ToListAsync();

            return View(poezii);
        }

        [HttpGet("Search")]
        public IActionResult Search()
        {
            return View();
        }

        [HttpPost("Search")]
        public async Task<IActionResult> Search(string titleSubstr, string volumeSubstr)
        {
            var poezii = await _context.Poezii.Include(p => p.Volum)
                .AsNoTracking()
                .Where(p => p.Titlu.Contains(titleSubstr) && p.Volum.Denumire.Contains(volumeSubstr))
                .ToListAsync();

            return View(poezii);
        }

        private Task<List<SelectListItem>> GetSelectListVolume()
        {
            var query = from v in _context.Volume
                        select new SelectListItem
                        {
                            Text = v.Denumire,
                            Value = v.Id.ToString()
                        };
            return query.ToListAsync();
        }
    }
}
