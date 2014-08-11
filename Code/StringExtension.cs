using System.Collections.Generic;
using System.Linq;

namespace Code
{
    public static class StringExtension
    {
        public static IEnumerable<int> ToNumList(this string s)
        {
            var result = new List<int>();
            if (!string.IsNullOrEmpty(s)) result.AddRange(s.Split(',').SelectMany(ParseRange));
            return result;
        }

        private static IEnumerable<int> ParseRange(string r)
        {
            var ps = r.Split('-');
            var a = int.Parse(ps[0]);
            var b = ps.Length == 2 ? int.Parse(ps[1]) : a;
            return Enumerable.Range(a, b - a + 1);
        }
    }
}
