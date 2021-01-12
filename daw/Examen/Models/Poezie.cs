using System.ComponentModel.DataAnnotations;

namespace Examen.Models
{
    public class Poezie
    {
        public int Id { get; set; }

        [Required]
        [Display(Name = "Titlu")]
        public string Titlu { get; set; }
        [Required]
        [Display(Name = "Autor")]
        public string Autor { get; set; }
        [Required]
        [Display(Name = "Număr de strofe")]
        public int NrStrofe { get; set; }

        [Display(Name = "Volum")]
        public int VolumId { get; set; }
        public Volum Volum { get; set; }
    }
}
