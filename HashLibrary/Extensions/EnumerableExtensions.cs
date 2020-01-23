// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections;
using System.Collections.Generic;

namespace Roslyn.Utilities
{
    internal static partial class EnumerableExtensions
    {
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE0059:Unnecessary assignment of a value", Justification = "required to detect empty enumerable")]
        public static bool IsEmpty<T>(this IEnumerable<T> source)
        {
            if (source is IReadOnlyCollection<T> readOnlyCollection)
                return readOnlyCollection.Count == 0;

            if (source is ICollection<T> genericCollection)
            {
                return genericCollection.Count == 0;
            }

            if (source is ICollection collection)
            {
                return collection.Count == 0;
            }

            if (source is string str)
            {
                return str.Length == 0;
            }

            foreach (T _ in source)
            {
                return false;
            }

            return true;
        }
    }
}
