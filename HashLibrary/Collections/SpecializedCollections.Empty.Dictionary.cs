// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;

namespace Roslyn.Utilities
{
    internal partial class SpecializedCollections
    {
        private partial class Empty
        {
            internal class Dictionary<TKey, TValue> : Collection<KeyValuePair<TKey, TValue>>, IDictionary<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>
            {
                [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "Public API")]
                public static new readonly Dictionary<TKey, TValue> Instance = new Dictionary<TKey, TValue>();

                private Dictionary()
                {
                }

                public void Add(TKey key, TValue value)
                {
                    throw new NotSupportedException();
                }

                public bool ContainsKey(TKey key)
                {
                    return false;
                }

                public ICollection<TKey> Keys => Collection<TKey>.Instance;

                IEnumerable<TKey> IReadOnlyDictionary<TKey, TValue>.Keys => Keys;
                IEnumerable<TValue> IReadOnlyDictionary<TKey, TValue>.Values => Values;

                public bool Remove(TKey key)
                {
                    throw new NotSupportedException();
                }

                public bool TryGetValue(TKey key, out TValue value)
                {
                    value = default;
                    return false;
                }

                public ICollection<TValue> Values => Collection<TValue>.Instance;

                public TValue this[TKey key]
                {
                    get => throw new NotSupportedException();

                    set => throw new NotSupportedException();
                }
            }
        }
    }
}
