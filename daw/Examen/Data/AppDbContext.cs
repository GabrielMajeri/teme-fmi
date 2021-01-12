using Examen.Models;
using Microsoft.EntityFrameworkCore;

namespace Examen.Data
{
    public class AppDbContext : DbContext
    {
        public DbSet<Poezie> Poezii { get; set; }

        public DbSet<Volum> Volume { get; set; }

        public AppDbContext(DbContextOptions<AppDbContext> options)
            : base(options)
        { }
    }
}
