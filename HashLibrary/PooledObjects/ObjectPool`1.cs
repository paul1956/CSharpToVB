﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

// define TRACE_LEAKS to get additional diagnostics that can lead to the leak sources. note: it will
// make everything about 2-3x slower
//
// #define TRACE_LEAKS

using System;
using System.Diagnostics;
using System.Threading;

namespace Microsoft.CodeAnalysis.PooledObjects1
{
    /// <summary>
    /// Generic implementation of object pooling pattern with predefined pool size limit. The main
    /// purpose is that limited number of frequently used objects can be kept in the pool for
    /// further recycling.
    ///
    /// Notes:
    /// 1) it is not the goal to keep all returned objects. Pool is not meant for storage. If there
    ///    is no space in the pool, extra returned objects will be dropped.
    ///
    /// 2) it is implied that if object was obtained from a pool, the caller will return it back in
    ///    a relatively short time. Keeping checked out objects for long durations is OK, but
    ///    reduces usefulness of pooling. Just new up your own.
    ///
    /// Not returning objects to the pool in not detrimental to the pool's work, but is a bad practice.
    /// Rationale:
    ///    If there is no intent for reusing the object, do not use pool - just use "new".
    /// </summary>
    public class ObjectPool<T> where T : class
    {
        // factory is stored for the lifetime of the pool. We will call this only when pool needs to
        // expand. compared to "new T()", Func gives more flexibility to implementers and faster
        // than "new T()".
        private readonly Factory _factory;

        private readonly Element[] _items;

        // Storage for the pool objects. The first item is stored in a dedicated field because we
        // expect to be able to satisfy most requests from it.
        private T _firstItem;

        public ObjectPool(Factory factory)
            : this(factory, Environment.ProcessorCount * 2)
        { }

        public ObjectPool(Factory factory, int size)
        {
            Debug.Assert(size >= 1);
            _factory = factory;
            _items = new Element[size - 1];
        }

        /// <remarks>
        /// Not using System.Func{T} because this file is linked into the (debugger) Formatter,
        /// which does not have that type (since it compiles against .NET 2.0).
        /// </remarks>
        public delegate T Factory();

        private T AllocateSlow()
        {
            var items = _items;

            for (int i = 0; i < items.Length; i++)
            {
                // Note that the initial read is optimistically not synchronized. That is intentional.
                // We will interlock only when we have a candidate. in a worst case we may miss some
                // recently returned objects. Not a big deal.
                T inst = items[i]._value;
                if (inst != null)
                {
                    if (inst == Interlocked.CompareExchange(ref items[i]._value, null, inst))
                    {
                        return inst;
                    }
                }
            }

            return CreateInstance();
        }

        private T CreateInstance()
        {
            var inst = _factory();
            return inst;
        }

        private void FreeSlow(T obj)
        {
            var items = _items;
            for (int i = 0; i < items.Length; i++)
            {
                if (items[i]._value == null)
                {
                    // Intentionally not using interlocked here.
                    // In a worst case scenario two objects may be stored into same slot.
                    // It is very unlikely to happen and will only mean that one of the objects will get collected.
                    items[i]._value = obj;
                    break;
                }
            }
        }

        [Conditional("DEBUG")]
        private void Validate(object obj)
        {
            Debug.Assert(obj != null, "freeing null?");

            Debug.Assert(_firstItem != obj, "freeing twice?");

            var items = _items;
            for (int i = 0; i < items.Length; i++)
            {
                var value = items[i]._value;
                if (value == null)
                {
                    return;
                }

                Debug.Assert(value != obj, "freeing twice?");
            }
        }

        /// <summary>
        /// Produces an instance.
        /// </summary>
        /// <remarks>
        /// Search strategy is a simple linear probing which is chosen for it cache-friendliness.
        /// Note that Free will try to store recycled objects close to the start thus statistically
        /// reducing how far we will typically search.
        /// </remarks>
        public T Allocate()
        {
            // PERF: Examine the first element. If that fails, AllocateSlow will look at the remaining elements.
            // Note that the initial read is optimistically not synchronized. That is intentional.
            // We will interlock only when we have a candidate. in a worst case we may miss some
            // recently returned objects. Not a big deal.
            T inst = _firstItem;
            if (inst == null || inst != Interlocked.CompareExchange(ref _firstItem, null, inst))
            {
                inst = AllocateSlow();
            }
            return inst;
        }

        /// <summary>
        /// Removes an object from leak tracking.
        ///
        /// This is called when an object is returned to the pool.  It may also be explicitly
        /// called if an object allocated from the pool is intentionally not being returned
        /// to the pool.  This can be of use with pooled arrays if the consumer wants to
        /// return a larger array to the pool than was originally allocated.
        /// </summary>
        [Conditional("DEBUG")]
        internal void ForgetTrackedObject(T old, T replacement = null)
        {
        }

        /// <summary>
        /// Returns objects to the pool.
        /// </summary>
        /// <remarks>
        /// Search strategy is a simple linear probing which is chosen for it cache-friendliness.
        /// Note that Free will try to store recycled objects close to the start thus statistically
        /// reducing how far we will typically search in Allocate.
        /// </remarks>
        public void Free(T obj)
        {
            Validate(obj);
            ForgetTrackedObject(obj);

            if (_firstItem == null)
            {
                // Intentionally not using interlocked here.
                // In a worst case scenario two objects may be stored into same slot.
                // It is very unlikely to happen and will only mean that one of the objects will get collected.
                _firstItem = obj;
            }
            else
            {
                FreeSlow(obj);
            }
        }

        [DebuggerDisplay("{Value,nq}")]
        private struct Element
        {
            internal T _value;
        }
    }
}
