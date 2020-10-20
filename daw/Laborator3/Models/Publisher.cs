using System.Collections.Generic;

namespace Laborator3.Models
{
    public class Publisher
    {
        public int PublisherId { get; set; }
        public string Name { get; set; }
        public string Address { get; set; }

        public ICollection<Edition> Editions { get; set; }
    }
}
