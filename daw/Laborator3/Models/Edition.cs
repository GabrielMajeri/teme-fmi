using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Laborator3.Models
{
    public class Edition
    {
        [Key]
        public string Isbn { get; set; }

        [Column(TypeName="Date")]
        [Required]
        public DateTime PublishDate { get; set; }

        public Book Book { get; set; }
        public Publisher Publisher { get; set; }
    }
}
