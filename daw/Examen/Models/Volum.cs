using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace Examen.Models
{
    public class Volum
    {
        public int Id { get; set; }

        [Required]
        public string Denumire { get; set; }

        public ICollection<Poezie> Poezii { get; set; }
    }
}
