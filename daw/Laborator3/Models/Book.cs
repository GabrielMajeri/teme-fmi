using System.Collections.Generic;

namespace Laborator3.Models
{
    /// Represents a written book.
    public class Book
    {
        public int BookId { get; set; }
        public string Title { get; set; }
        public string Author { get; set; }
        public string Summary { get; set; }

        public ICollection<Edition> Editions { get; set; }
    }
}
