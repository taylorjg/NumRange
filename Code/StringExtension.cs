using System.Collections.Generic;
using System.Linq;

namespace Code
{
    public static class StringExtension
    {
        public static IEnumerable<int> ToNumList(this string s)
        {
            return !string.IsNullOrEmpty(s) ? s.Split(',').SelectMany(r =>
                {
                    var ns = r.Split('-').Select(int.Parse).ToArray();
                    var a = ns[0];
                    var b = ns.Length > 1 ? ns[1] : a;
                    return Enumerable.Range(a, b - a + 1);
                }) : Enumerable.Empty<int>();
        }
    }
}
