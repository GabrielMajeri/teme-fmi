using Microsoft.EntityFrameworkCore;

namespace Examen.Data
{
    public class AppDbContext : DbContext
    {
        public AppDbContext(DbContextOptions<AppDbContext> options)
            : base(options)
        { }

        public void Seed()
        {
            if (!Database.EnsureCreated())
            {
                return;
            }
        }
    }
}
