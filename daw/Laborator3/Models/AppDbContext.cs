using System;
using Microsoft.EntityFrameworkCore;

namespace Laborator3.Models
{
    public class AppDbContext : DbContext
    {
        public DbSet<Book> Books { get; set; }

        /// Delete and recreate the database.
        public void DropCreate()
        {
            Database.EnsureDeleted();
            Database.EnsureCreated();

            SaveChanges();
        }

        /// Insert some seed data.
        public void Seed()
        {
            Add(new Book {
                BookId = 1,
                Title = "Testing Book",
                Author = "John",
                Summary = "How to write automated unit tests"
            });

            Add(new Book {
                BookId = 2,
                Title = "Jokes",
                Author = "Victor"
            });

            var book = new Book {
                BookId = 15,
                Title = "Poezii",
                Author = "Eminescu",
                Summary = "Cele mai frumoase cuvinte"
            };
            Add(book);

            var publisher = new Publisher {
                PublisherId = 1,
                Name = "Paralela 45",
                Address = "București"
            };
            Add(publisher);

            Add(new Edition {
                Isbn = "978-8-6757-3633-2",
                Book = book,
                Publisher = publisher,
                PublishDate = DateTime.Now
            });

            SaveChanges();
        }

        protected override void OnConfiguring(DbContextOptionsBuilder options)
            => options.UseSqlite("Data Source=books.db");
    }
}
