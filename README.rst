Vodka
=====

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/vodka.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/vodka


Setters
-------

.. code-block:: erlang

    Rec#rec{FieldName = FieldValue}

Getters
-------

.. code-block:: erlang

    get_multi_field(K1, K2, K3, A) ->
        #rec{K1 = V} = #rec{K2 = V} = A#rec{K3 = updated},
        V.

    get_nested_field(K1, A) ->
       [#rec{K1 = V}] = A,
       V.


Problems
--------

It does not work with query comprehensions.
