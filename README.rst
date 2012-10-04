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

Proplist to record
------------------

.. code-block:: erlang

    pl_to_record(PL) ->
        [Rec#rec{K = V} || {K, V} <- PL].
    
Getters
-------

Not yet implemented.

