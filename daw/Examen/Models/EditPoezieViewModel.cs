using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;

namespace Examen.Models
{
    public class EditPoezieViewModel
    {
        [Required]
        public Poezie Poezie { get; set; }
        public ICollection<SelectListItem> Volume { get; set; }
    }
}
