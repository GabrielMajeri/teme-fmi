using Examen.Models;

namespace Examen.Data
{
    public class DatabaseInitializer
    {
        private readonly AppDbContext _context;

        public DatabaseInitializer(AppDbContext context)
        {
            _context = context;
        }

        public void Seed()
        {
            if (_context.Database.EnsureCreated() == false)
            {
                // Deja exista baza de date
                return;
            }

            var volume = new[]
            {
                new Volum { Denumire = "Cuvinte potrivite" },
                new Volum { Denumire = "Poezii alese" },
                new Volum { Denumire = "Cele mai bune poezii" },
            };
            _context.AddRange(volume);

            var poezii = new[]
            {
                new Poezie
                {
                    Titlu = "Poem",
                    Autor = "Mihai Eminescu",
                    NrStrofe = 11,
                    Volum = volume[1],
                },
                new Poezie
                {
                    Titlu = "Odă",
                    Autor = "Mihai Eminescu",
                    NrStrofe = 4,
                    Volum = volume[1],
                },
                new Poezie
                {
                    Titlu = "Ars poetica",
                    Autor = "Tudor Arghezi",
                    NrStrofe = 3,
                    Volum = volume[0],
                },
                new Poezie
                {
                    Titlu = "Flori de mucigai",
                    Autor = "Tudor Arghezi",
                    NrStrofe = 8,
                    Volum = volume[2],
                },
                new Poezie
                {
                    Titlu = "Floare albstră",
                    Autor = "Mihai Eminescu",
                    NrStrofe = 6,
                    Volum = volume[2],
                },
                new Poezie
                {
                    Titlu = "Ars poetica",
                    Autor = "Nichita Stănescu",
                    NrStrofe = 2,
                    Volum = volume[2],
                }
            };
            _context.AddRange(poezii);

            _context.SaveChanges();
        }
    }
}
